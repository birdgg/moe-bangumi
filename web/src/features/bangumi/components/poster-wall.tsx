import { useMemo, useState } from "react";
import { useTrackedBangumis } from "../hooks/use-tracked-bangumis";
import { getCurrentSeason, getSeasonFromDate } from "@/features/calendar";
import { Skeleton } from "@/components/ui/skeleton";
import { TrackingCard } from "./tracking-card";
import { TrackingModal } from "./tracking-modal";
import type { TrackingWithBangumiResponse } from "@/client/types.gen";

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

function BangumiGrid({
  items,
  onSelect,
}: {
  items: TrackingWithBangumiResponse[];
  onSelect: (id: number) => void;
}) {
  return (
    <div className="flex flex-wrap gap-4">
      {items.map((item, i) => (
        <TrackingCard
          key={item.tracking.id}
          item={item}
          index={i}
          onClick={() => onSelect(item.tracking.id)}
        />
      ))}
    </div>
  );
}

export function PosterWall() {
  const { data: trackings, isLoading, error } = useTrackedBangumis();
  const [selectedId, setSelectedId] = useState<number | null>(null);
  const selected = trackings?.find((t) => t.tracking.id === selectedId) ?? null;

  const { current, historical } = useMemo(() => {
    if (!trackings) return { current: [], historical: [] };
    const { year, season } = getCurrentSeason();
    const cur: TrackingWithBangumiResponse[] = [];
    const hist: TrackingWithBangumiResponse[] = [];
    for (const item of trackings) {
      const s = getSeasonFromDate(item.bangumi.airDate);
      if (s.year === year && s.season === season) {
        cur.push(item);
      } else {
        hist.push(item);
      }
    }
    return { current: cur, historical: hist };
  }, [trackings]);

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

  const handleSelect = (id: number) => setSelectedId(id);

  return (
    <>
      {current.length > 0 && (
        <section>
          <h2 className="text-base font-semibold text-foreground/80 mb-4">
            当季追番
          </h2>
          <BangumiGrid items={current} onSelect={handleSelect} />
        </section>
      )}

      {historical.length > 0 && (
        <section className={current.length > 0 ? "mt-8" : ""}>
          <h2 className="text-base font-semibold text-foreground/80 mb-4">
            历史番剧
          </h2>
          <BangumiGrid items={historical} onSelect={handleSelect} />
        </section>
      )}

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
