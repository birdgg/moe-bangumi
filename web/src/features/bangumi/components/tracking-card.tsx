import { motion } from "framer-motion";
import type { TrackingWithBangumiResponse } from "@/client/types.gen";

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

export function TrackingCard({
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
          <div className="flex size-full items-center justify-center bg-linear-to-br from-muted to-muted/50">
            <span className="text-muted-foreground text-xs">No Image</span>
          </div>
        )}

        {/* Subscription badge */}
        {tracking.trackingType === "subscription" && (
          <div className="absolute top-2 left-2 z-10">
            <span className="inline-flex items-center rounded-md bg-black/50 px-1.5 py-0.5 text-[10px] font-medium text-white/90 backdrop-blur-sm ring-1 ring-white/10">
              订阅中
            </span>
          </div>
        )}

        {/* Cinematic gradient scrim */}
        <div className="absolute inset-x-0 bottom-0 h-3/5 bg-linear-to-t from-black/85 via-black/40 to-transparent pointer-events-none" />

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
          </div>
        </div>

        {/* Progress bar */}
        {progress !== null && (
          <div className="absolute inset-x-0 bottom-0 h-0.75 bg-white/5">
            <motion.div
              className="h-full bg-linear-to-r from-chart-1 to-chart-2"
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
