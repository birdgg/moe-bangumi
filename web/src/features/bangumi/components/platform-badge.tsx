import { cn } from "@/lib/utils";
import { IconDeviceTv, IconWorld, IconMovie } from "@tabler/icons-react";

interface PlatformBadgeProps {
  platform: string | null | undefined;
  variant?: "default" | "overlay";
  className?: string;
}

function getPlatformIcon(platform: string | null | undefined, variant: "default" | "overlay") {
  const iconClass = variant === "overlay" ? "size-3" : "size-3.5";
  switch (platform) {
    case "TV":
      return <IconDeviceTv className={iconClass} />;
    case "Web":
      return <IconWorld className={iconClass} />;
    case "剧场版":
      return <IconMovie className={iconClass} />;
    default:
      return null;
  }
}

export function PlatformBadge({ platform, variant = "default", className }: PlatformBadgeProps) {
  if (!platform) return null;

  return (
    <span
      className={cn(
        "inline-flex items-center gap-1 font-medium",
        variant === "default" && [
          "px-2 py-0.5 rounded-full text-xs",
          "bg-chart-1/15 text-chart-2",
          "dark:bg-chart-1/20 dark:text-chart-1",
        ],
        variant === "overlay" && [
          "px-1 py-0.5 rounded text-[9px] font-bold",
          "bg-white/20 text-white backdrop-blur-sm",
        ],
        className
      )}
    >
      {getPlatformIcon(platform, variant)}
      {platform}
    </span>
  );
}
