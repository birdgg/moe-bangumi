use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::mpsc;

use super::handle::SchedulerHandle;
use super::messages::{JobStatus, SchedulerError, SchedulerMessage};
use crate::services::scheduler::traits::SchedulerJob;

/// 单个 Job 的运行时状态
struct JobEntry {
    job: Arc<dyn SchedulerJob>,
    is_running: bool,
}

/// Scheduler Actor
pub struct SchedulerActor {
    jobs: HashMap<&'static str, JobEntry>,
    receiver: mpsc::Receiver<SchedulerMessage>,
}

impl SchedulerActor {
    pub fn new(
        jobs: Vec<Arc<dyn SchedulerJob>>,
        receiver: mpsc::Receiver<SchedulerMessage>,
    ) -> Self {
        let mut job_map = HashMap::new();

        for job in jobs {
            let name = job.name();
            job_map.insert(
                name,
                JobEntry {
                    job,
                    is_running: false,
                },
            );
        }

        Self {
            jobs: job_map,
            receiver,
        }
    }

    /// 启动各 Job 的定时任务
    pub fn spawn_timers(&self, handle: SchedulerHandle) {
        for (name, entry) in &self.jobs {
            let handle = handle.clone();
            let interval = entry.job.interval();
            let job_name = *name;

            tokio::spawn(async move {
                let mut timer = tokio::time::interval(interval);
                timer.set_missed_tick_behavior(tokio::time::MissedTickBehavior::Skip);

                loop {
                    timer.tick().await;
                    handle.send_timer_tick(job_name).await;
                }
            });
        }
    }

    /// 运行 Actor 主循环
    pub async fn run(mut self) {
        tracing::info!("Scheduler actor started with {} jobs", self.jobs.len());

        while let Some(msg) = self.receiver.recv().await {
            self.handle_message(msg).await;
        }

        tracing::info!("Scheduler actor stopped");
    }

    /// 处理消息
    async fn handle_message(&mut self, msg: SchedulerMessage) {
        match msg {
            SchedulerMessage::TriggerJob { job_name, reply } => {
                let result = self.trigger_job_by_name(&job_name).await;
                let _ = reply.send(result);
            }

            SchedulerMessage::ListJobs { reply } => {
                let statuses = self.get_job_statuses();
                let _ = reply.send(statuses);
            }

            SchedulerMessage::TimerTick { job_name } => {
                self.execute_job(job_name).await;
            }
        }
    }

    /// 按名称触发 Job
    async fn trigger_job_by_name(&mut self, job_name: &str) -> Result<(), SchedulerError> {
        // 查找 Job
        let entry = self
            .jobs
            .iter()
            .find(|(name, _)| **name == job_name)
            .map(|(name, _)| *name);

        match entry {
            Some(name) => {
                let job_entry = self.jobs.get(name).unwrap();
                if job_entry.is_running {
                    Err(SchedulerError::JobAlreadyRunning(job_name.to_string()))
                } else {
                    self.execute_job(name).await;
                    Ok(())
                }
            }
            None => Err(SchedulerError::JobNotFound(job_name.to_string())),
        }
    }

    /// 执行指定 Job
    async fn execute_job(&mut self, name: &'static str) {
        let entry = match self.jobs.get_mut(name) {
            Some(e) => e,
            None => return,
        };

        // 检查是否正在运行
        if entry.is_running {
            tracing::debug!("Job '{}' is already running, skipping this trigger", name);
            return;
        }

        // 标记为运行中
        entry.is_running = true;
        let job = Arc::clone(&entry.job);

        // 执行 Job
        let result = job.execute().await;

        // 更新状态
        if let Some(entry) = self.jobs.get_mut(name) {
            entry.is_running = false;
        }

        match result {
            Ok(()) => tracing::debug!("Job '{}' completed successfully", name),
            Err(e) => tracing::error!("Job '{}' failed: {}", name, e),
        }
    }

    /// 获取所有 Job 状态
    fn get_job_statuses(&self) -> Vec<JobStatus> {
        self.jobs
            .iter()
            .map(|(name, entry)| JobStatus {
                name,
                interval_secs: entry.job.interval().as_secs(),
                is_running: entry.is_running,
            })
            .collect()
    }
}
