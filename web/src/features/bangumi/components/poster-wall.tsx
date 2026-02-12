import { useState } from "react";
import { motion } from "framer-motion";
import { useTrackedBangumis } from "../hooks/use-tracked-bangumis";
import { Skeleton } from "@/components/ui/skeleton";
import type { TrackingWithBangumiResponse } from "@/client/types.gen";
import { TrackingModal } from "./tracking-modal";

const cardVariants = {
  hidden: { opacity: 0, y: 16, scale: 0.96 },
  visible: (i: number) => ({
    opacity: 1,
    y: 0,
    scale: 1,
    transition: {
      delay: i * 0.04,
      duration: 0.4,
      ease: [0.33, 1, 0.68, 1] as const,
    },
  }),
};

function PosterCard({
  item,
  index,
  onClick,
}: {
  item: TrackingWithBangumiResponse;
  index: number;
  onClick: () => void;
}) {
  const { bangumi, tracking } = item;

  const seasonText =
    bangumi.kind === "tv" && bangumi.season && bangumi.season > 1
      ? ` 第${bangumi.season}季`
      : "";

  const progress =
    bangumi.totalEpisodes && bangumi.totalEpisodes > 0
      ? Math.min(tracking.currentEpisode / bangumi.totalEpisodes, 1)
      : null;

  return (
    <motion.div
      className="group relative w-[13rem] cursor-pointer"
      onClick={onClick}
      custom={index}
      variants={cardVariants}
      initial="hidden"
      animate="visible"
      whileHover={{ y: -6 }}
      whileTap={{ scale: 0.97 }}
      transition={{ type: "spring", stiffness: 300, damping: 22 }}
    >
      <div className="poster-card relative aspect-[2/3] overflow-hidden rounded-xl">
        {bangumi.posterUrl ? (
          <img
            src={bangumi.posterUrl}
            alt={bangumi.titleChs}
            className="size-full object-cover transition-transform duration-500 ease-out group-hover:scale-[1.06]"
          />
        ) : (
          <div className="flex size-full items-center justify-center bg-gradient-to-br from-muted to-muted/50">
            <span className="text-muted-foreground text-xs">No Image</span>
          </div>
        )}

        {/* Cinematic gradient scrim */}
        <div className="absolute inset-x-0 bottom-0 h-3/5 bg-gradient-to-t from-black/85 via-black/40 to-transparent pointer-events-none" />

        {/* Hover vignette */}
        <div className="absolute inset-0 bg-black/0 group-hover:bg-black/10 transition-colors duration-300 pointer-events-none" />

        {/* Title + meta */}
        <div className="absolute inset-x-0 bottom-0 p-3 pb-3.5">
          <h3 className="line-clamp-2 text-xs font-semibold leading-snug text-white drop-shadow-[0_1px_3px_rgba(0,0,0,0.6)]">
            {bangumi.titleChs}
            {seasonText && (
              <span className="text-white/50 font-normal">{seasonText}</span>
            )}
          </h3>

          <div className="flex items-center gap-2 mt-1.5 translate-y-1 opacity-0 transition-all duration-300 ease-out group-hover:translate-y-0 group-hover:opacity-100">
            {progress !== null && (
              <span className="text-[11px] font-medium tabular-nums text-white/70">
                EP {tracking.currentEpisode}
                <span className="text-white/40">
                  /{bangumi.totalEpisodes}
                </span>
              </span>
            )}
            {tracking.isBDrip && (
              <span className="text-[11px] font-medium text-blue-400">
                BD
              </span>
            )}
            {tracking.trackingType === "subscription" &&
              tracking.rssEnabled && (
                <span className="text-[11px] font-medium text-chart-1">
                  RSS
                </span>
              )}
          </div>
        </div>

        {/* Progress bar */}
        {progress !== null && (
          <div className="absolute inset-x-0 bottom-0 h-[3px] bg-white/5">
            <motion.div
              className="h-full bg-gradient-to-r from-chart-1 to-chart-2"
              initial={{ width: 0 }}
              animate={{ width: `${progress * 100}%` }}
              transition={{
                duration: 0.6,
                ease: [0.33, 1, 0.68, 1],
                delay: 0.15 + index * 0.04,
              }}
            />
          </div>
        )}
      </div>
    </motion.div>
  );
}

function PosterWallSkeleton() {
  return (
    <div className="flex flex-wrap gap-4">
      {Array.from({ length: 6 }).map((_, i) => (
        <Skeleton
          key={i}
          className="aspect-[2/3] w-[13rem] rounded-xl"
        />
      ))}
    </div>
  );
}

function EmptyState() {
  return (
    <div className="flex flex-col items-center justify-center py-16 text-center">
      <div className="rounded-full bg-muted p-4 mb-4">
        <svg
          className="size-8 text-muted-foreground"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          strokeWidth="1.5"
        >
          <path d="M19 11H5m14 0a2 2 0 012 2v6a2 2 0 01-2 2H5a2 2 0 01-2-2v-6a2 2 0 012-2m14 0V9a2 2 0 00-2-2M5 11V9a2 2 0 012-2m0 0V5a2 2 0 012-2h6a2 2 0 012 2v2M7 7h10" />
        </svg>
      </div>
      <h3 className="text-lg font-medium text-foreground/90 mb-1">
        还没有追番
      </h3>
      <p className="text-sm text-muted-foreground">
        去日历页面订阅你喜欢的番剧吧
      </p>
    </div>
  );
}

export function PosterWall() {
  const { data: trackings, isLoading, error } = useTrackedBangumis();
  const [selectedId, setSelectedId] = useState<number | null>(null);
  const selected = trackings?.find((t) => t.tracking.id === selectedId) ?? null;

  if (isLoading) {
    return <PosterWallSkeleton />;
  }

  if (error) {
    return (
      <div className="rounded-lg bg-destructive/10 p-4 text-center text-destructive">
        加载失败: {error.message}
      </div>
    );
  }

  if (!trackings || trackings.length === 0) {
    return <EmptyState />;
  }

  return (
    <>
      <div className="flex flex-wrap gap-4">
        {trackings.map((item, i) => (
          <PosterCard
            key={item.tracking.id}
            item={item}
            index={i}
            onClick={() => setSelectedId(item.tracking.id)}
          />
        ))}
      </div>

      {selected && (
        <TrackingModal
          open={!!selected}
          onOpenChange={(open) => {
            if (!open) setSelectedId(null);
          }}
          bangumi={selected.bangumi}
          tracking={selected.tracking}
        />
      )}
    </>
  );
}
