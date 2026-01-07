//! Periodic Actor Infrastructure
//!
//! Provides common infrastructure for timer-based background actors.

use std::time::Duration;
use tokio::sync::mpsc;
use tokio::time::MissedTickBehavior;

/// Message types for periodic actors
pub enum ActorMessage {
    Shutdown,
}

/// Trait for periodic background actors
///
/// Implement this trait to create a periodic actor that executes
/// at regular intervals.
pub trait PeriodicActor: Send + 'static {
    /// The interval between executions
    fn interval(&self) -> Duration;

    /// The actor name (used for logging)
    fn name(&self) -> &'static str;

    /// Execute the periodic task
    ///
    /// This method is called at each interval tick.
    fn execute(&mut self) -> impl std::future::Future<Output = ()> + Send;
}

/// Handle for communicating with a periodic actor
#[derive(Clone)]
pub struct ActorHandle {
    sender: mpsc::Sender<ActorMessage>,
}

impl ActorHandle {
    /// Signal the actor to shutdown
    pub async fn shutdown(&self) {
        let _ = self.sender.send(ActorMessage::Shutdown).await;
    }
}

/// Spawn a periodic actor and return a handle to communicate with it
///
/// The actor will execute at regular intervals defined by `PeriodicActor::interval()`.
/// The first execution is delayed by one interval (immediate tick is skipped).
pub fn spawn_periodic_actor<A: PeriodicActor>(mut actor: A) -> ActorHandle {
    let (sender, mut receiver) = mpsc::channel(8);

    tokio::spawn(async move {
        let mut timer = tokio::time::interval(actor.interval());
        timer.set_missed_tick_behavior(MissedTickBehavior::Skip);

        // Skip first tick (immediate)
        timer.tick().await;

        loop {
            tokio::select! {
                _ = timer.tick() => {
                    actor.execute().await;
                }
                msg = receiver.recv() => {
                    match msg {
                        Some(ActorMessage::Shutdown) | None => {
                            tracing::info!("{} actor stopped", actor.name());
                            break;
                        }
                    }
                }
            }
        }
    });

    ActorHandle { sender }
}
