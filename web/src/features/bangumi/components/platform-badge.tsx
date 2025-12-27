import { cn } from "@/lib/utils";
import { IconDeviceTv, IconMovie, IconVideoPlus } from "@tabler/icons-react";
import type { Platform } from "@/lib/api";

interface PlatformBadgeProps {
  platform: Platform | string | null | undefined;
  variant?: "default" | "overlay";
  className?: string;
}

const PLATFORM_CONFIG = {
  tv: { icon: IconDeviceTv, label: "TV" },
  movie: { icon: IconMovie, label: "Movie" },
  ova: { icon: IconVideoPlus, label: "OVA" },
} as const;

// Map external platform strings to our Platform enum
// BGM.tv uses "TV", "剧场版", "OVA" etc.
function normalizePlatform(platform: string | null | undefined): Platform | null {
  if (!platform) return null;
  const lower = platform.toLowerCase();
  if (lower === "tv" || lower === "web") return "tv";
  if (lower === "movie" || lower === "剧场版" || lower === "劇場版") return "movie";
  if (lower === "ova" || lower === "oad") return "ova";
  return null;
}

function getPlatformIcon(platform: Platform | null, variant: "default" | "overlay") {
  if (!platform) return null;
  const iconClass = variant === "overlay" ? "size-3" : "size-3.5";
  const Icon = PLATFORM_CONFIG[platform].icon;
  return <Icon className={iconClass} />;
}

function getPlatformLabel(platform: Platform | null): string | null {
  if (!platform) return null;
  return PLATFORM_CONFIG[platform].label;
}

export function PlatformBadge({ platform: rawPlatform, variant = "default", className }: PlatformBadgeProps) {
  const platform = normalizePlatform(rawPlatform);
  const label = getPlatformLabel(platform);
  if (!label) return null;

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
      {label}
    </span>
  );
}
