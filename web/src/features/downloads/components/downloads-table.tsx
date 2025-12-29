import { useState } from "react";
import { IconTrash, IconLoader2 } from "@tabler/icons-react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from "@/components/ui/alert-dialog";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "@/components/ui/tooltip";
import type { Task } from "../hooks/use-downloads";
import type { TaskStatus } from "@/lib/api";

interface DownloadsTableProps {
  tasks: Task[];
  onDelete: (hashes: string[]) => void;
  isDeleting?: boolean;
}

// Format bytes to human readable
function formatBytes(bytes: number): string {
  if (bytes === 0) return "0 B";
  const k = 1024;
  const sizes = ["B", "KB", "MB", "GB", "TB"];
  const i = Math.floor(Math.log(bytes) / Math.log(k));
  return `${parseFloat((bytes / Math.pow(k, i)).toFixed(1))} ${sizes[i]}`;
}

// Get status display info based on TaskStatus
function getStatusInfo(status: TaskStatus): { label: string; color: string } {
  const statusMap: Record<TaskStatus, { label: string; color: string }> = {
    downloading: { label: "下载中", color: "text-chart-1" },
    seeding: { label: "做种", color: "text-chart-3" },
    completed: { label: "已完成", color: "text-chart-3" },
    paused: { label: "已暂停", color: "text-muted-foreground" },
    checking: { label: "检查中", color: "text-chart-2" },
    queued: { label: "排队中", color: "text-muted-foreground" },
    stalled: { label: "等待中", color: "text-chart-5" },
    error: { label: "错误", color: "text-destructive" },
    unknown: { label: "未知", color: "text-muted-foreground" },
  };

  return statusMap[status] || { label: status, color: "text-muted-foreground" };
}

export function DownloadsTable({
  tasks,
  onDelete,
  isDeleting,
}: DownloadsTableProps) {
  const [deleteTarget, setDeleteTarget] = useState<string | null>(null);

  const handleDelete = () => {
    if (deleteTarget) {
      onDelete([deleteTarget]);
      setDeleteTarget(null);
    }
  };

  return (
    <>
      <div className="overflow-x-auto rounded-xl border border-border/50 bg-card/50 backdrop-blur-sm">
        <table className="w-full text-sm">
          <thead>
            <tr className="border-b border-border/50 text-left text-muted-foreground">
              <th className="px-4 py-3 font-medium">名称</th>
              <th className="px-4 py-3 font-medium w-24">状态</th>
              <th className="px-4 py-3 font-medium w-40">进度</th>
              <th className="px-4 py-3 font-medium w-32 text-right">大小</th>
              <th className="px-4 py-3 font-medium w-20 text-right">操作</th>
            </tr>
          </thead>
          <tbody>
            {tasks.map((task) => {
              const status = getStatusInfo(task.status);
              const progress = Math.round(task.progress * 100);

              return (
                <tr
                  key={task.id}
                  className="border-b border-border/30 last:border-0 hover:bg-muted/30 transition-colors"
                >
                  {/* Name */}
                  <td className="px-4 py-3">
                    <Tooltip>
                      <TooltipTrigger
                        render={
                          <span className="block truncate text-foreground cursor-default" />
                        }
                      >
                        {task.name}
                      </TooltipTrigger>
                      <TooltipContent side="top" className="max-w-md">
                        <p className="break-all">{task.name}</p>
                      </TooltipContent>
                    </Tooltip>
                  </td>

                  {/* Status */}
                  <td className="px-4 py-3">
                    <span className={cn("font-medium", status.color)}>
                      {status.label}
                    </span>
                  </td>

                  {/* Progress */}
                  <td className="px-4 py-3">
                    <div className="flex items-center gap-2">
                      <div className="h-2 flex-1 rounded-full bg-muted/50 overflow-hidden">
                        <div
                          className={cn(
                            "h-full rounded-full transition-all",
                            progress === 100
                              ? "bg-chart-3"
                              : "bg-linear-to-r from-chart-1 to-chart-3"
                          )}
                          style={{ width: `${progress}%` }}
                        />
                      </div>
                      <span className="text-xs text-muted-foreground w-10 text-right">
                        {progress}%
                      </span>
                    </div>
                  </td>

                  {/* Size */}
                  <td className="px-4 py-3 text-right text-muted-foreground">
                    {formatBytes(task.total_size)}
                  </td>

                  {/* Actions */}
                  <td className="px-4 py-3">
                    <div className="flex items-center justify-end">
                      <Tooltip>
                        <TooltipTrigger
                          render={
                            <Button
                              variant="ghost"
                              size="icon-sm"
                              onClick={() => setDeleteTarget(task.id)}
                              disabled={isDeleting}
                              className="text-destructive hover:text-destructive hover:bg-destructive/10"
                            />
                          }
                        >
                          {isDeleting ? (
                            <IconLoader2 className="size-4 animate-spin" />
                          ) : (
                            <IconTrash className="size-4" />
                          )}
                        </TooltipTrigger>
                        <TooltipContent>删除</TooltipContent>
                      </Tooltip>
                    </div>
                  </td>
                </tr>
              );
            })}
          </tbody>
        </table>
      </div>

      {/* Delete confirmation dialog */}
      <AlertDialog
        open={!!deleteTarget}
        onOpenChange={() => setDeleteTarget(null)}
      >
        <AlertDialogContent>
          <AlertDialogHeader>
            <AlertDialogTitle>确认删除</AlertDialogTitle>
            <AlertDialogDescription>
              确定要删除此下载任务吗？这将同时删除已下载的文件。
            </AlertDialogDescription>
          </AlertDialogHeader>
          <AlertDialogFooter>
            <AlertDialogCancel>取消</AlertDialogCancel>
            <AlertDialogAction
              onClick={handleDelete}
              className="bg-destructive text-destructive-foreground hover:bg-destructive/90"
            >
              删除
            </AlertDialogAction>
          </AlertDialogFooter>
        </AlertDialogContent>
      </AlertDialog>
    </>
  );
}
