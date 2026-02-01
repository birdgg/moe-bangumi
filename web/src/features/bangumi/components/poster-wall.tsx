import { useTrackedBangumis } from "../hooks/use-tracked-bangumis";
import { Skeleton } from "@/components/ui/skeleton";
import type { TrackingWithBangumiResponse } from "@/client/types.gen";

function PosterCard({ item }: { item: TrackingWithBangumiResponse }) {
  const { bangumi, tracking } = item;

  const seasonText =
    bangumi.kind === "tv" && bangumi.season && bangumi.season > 1
      ? ` S${bangumi.season}`
      : "";

  return (
    <div className="group w-44">
      <div className="liquid-glass-card relative overflow-hidden rounded-2xl">
        <div className="liquid-glass-refraction" />

        <div className="aspect-[2/3] overflow-hidden rounded-t-2xl relative">
          {bangumi.posterUrl ? (
            <img
              src={bangumi.posterUrl}
              alt={bangumi.titleChs}
              className="size-full object-cover transition-transform duration-300 ease-out group-hover:scale-110"
            />
          ) : (
            <div className="flex size-full items-center justify-center bg-gradient-to-br from-muted to-muted/50">
              <span className="text-muted-foreground text-sm">No Image</span>
            </div>
          )}

          <div className="absolute bottom-0 left-0 right-0 bg-gradient-to-t from-black/80 to-transparent p-3 pt-8">
            <div className="flex items-center gap-1.5">
              <span className="rounded bg-primary/90 px-1.5 py-0.5 text-xs font-medium text-primary-foreground">
                EP {tracking.currentEpisode}
              </span>
              {tracking.trackingType === "Collection" && (
                <span className="rounded bg-amber-500/90 px-1.5 py-0.5 text-xs font-medium text-white">
                  收藏
                </span>
              )}
            </div>
          </div>
        </div>

        <div className="liquid-glass-content relative h-12 p-2.5">
          <h3 className="line-clamp-2 text-xs font-medium leading-snug text-foreground/90">
            {bangumi.titleChs}
            {seasonText}
          </h3>
        </div>
      </div>
    </div>
  );
}

function PosterWallSkeleton() {
  return (
    <div className="flex flex-wrap gap-4">
      {Array.from({ length: 6 }).map((_, i) => (
        <Skeleton key={i} className="h-64 w-44 rounded-2xl" />
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
    <div className="flex flex-wrap gap-4">
      {trackings.map((item) => (
        <PosterCard key={item.tracking.id} item={item} />
      ))}
    </div>
  );
}
