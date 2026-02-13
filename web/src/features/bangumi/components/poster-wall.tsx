import { useState } from "react";
import { useTrackedBangumis } from "../hooks/use-tracked-bangumis";
import { Skeleton } from "@/components/ui/skeleton";
import { TrackingCard } from "./tracking-card";
import { TrackingModal } from "./tracking-modal";

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
          <TrackingCard
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
