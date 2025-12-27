import { LogItem } from "./log-item";

interface Log {
  id: number;
  created_at: string;
  level: "info" | "warning" | "error";
  message: string;
}

interface LogListProps {
  logs: Log[];
}

export function LogList({ logs }: LogListProps) {
  if (logs.length === 0) {
    return (
      <div className="py-12 text-center text-muted-foreground font-mono text-sm">
        暂无日志记录
      </div>
    );
  }

  return (
    <div className="divide-y divide-muted/30">
      {logs.map((log) => (
        <LogItem key={log.id} log={log} />
      ))}
    </div>
  );
}
