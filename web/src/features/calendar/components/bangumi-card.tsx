import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import {
  deleteApiTrackingByIdMutation,
  getApiTrackingOptions,
  postApiTrackingMutation,
} from "@/client/@tanstack/react-query.gen";
import type { BangumiResponse } from "@/client/types.gen";

interface BangumiCardProps {
  bangumi: BangumiResponse;
}

export function BangumiCard({ bangumi }: BangumiCardProps) {
  const queryClient = useQueryClient();

  const { data: trackings } = useQuery(getApiTrackingOptions());

  const tracking = trackings?.find((t) => t.bangumiId === bangumi.id);
  const isSubscribed = !!tracking;

  const subscribeMutation = useMutation({
    ...postApiTrackingMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["getApiTracking"] });
    },
  });

  const unsubscribeMutation = useMutation({
    ...deleteApiTrackingByIdMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["getApiTracking"] });
    },
  });

  const isPending = subscribeMutation.isPending || unsubscribeMutation.isPending;

  const handleSubscribe = (e: React.MouseEvent) => {
    e.stopPropagation();
    if (!bangumi.id) return;

    if (isSubscribed && tracking) {
      unsubscribeMutation.mutate({
        path: { id: tracking.id },
      });
    } else {
      subscribeMutation.mutate({
        body: {
          bangumiId: bangumi.id,
          mikanId: bangumi.mikanId,
          trackingType: "Subscription",
        },
      });
    }
  };

  const seasonText =
    bangumi.kind === "tv" && bangumi.season && bangumi.season > 1
      ? ` 第${bangumi.season}季`
      : "";

  return (
    <div className="group w-52">
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

          {bangumi.mikanId && (
            <div className="absolute top-2 right-2 z-10">
              <button
                type="button"
                onClick={handleSubscribe}
                disabled={isPending}
                className={`subscribe-btn-corner ${isSubscribed ? "subscribed" : "opacity-0 group-hover:opacity-100"} transition-all duration-300`}
              >
                {isPending ? (
                  <svg
                    className="size-3.5 animate-spin"
                    viewBox="0 0 24 24"
                    fill="none"
                  >
                    <circle
                      className="opacity-25"
                      cx="12"
                      cy="12"
                      r="10"
                      stroke="currentColor"
                      strokeWidth="3"
                    />
                    <path
                      className="opacity-75"
                      fill="currentColor"
                      d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                    />
                  </svg>
                ) : isSubscribed ? (
                  <>
                    <svg className="size-3.5" viewBox="0 0 24 24" fill="currentColor">
                      <path d="M9 16.17L4.83 12l-1.42 1.41L9 19 21 7l-1.41-1.41L9 16.17z" />
                    </svg>
                    <span>已订阅</span>
                  </>
                ) : (
                  <>
                    <svg
                      className="size-3.5"
                      viewBox="0 0 24 24"
                      fill="none"
                      stroke="currentColor"
                      strokeWidth="2.5"
                    >
                      <path d="M12 5v14M5 12h14" />
                    </svg>
                    <span>订阅</span>
                  </>
                )}
              </button>
            </div>
          )}
        </div>

        <div className="liquid-glass-content relative h-14 p-3">
          <h3 className="line-clamp-2 text-sm font-medium leading-snug text-foreground/90">
            {bangumi.titleChs}
            {seasonText}
          </h3>
        </div>
      </div>
    </div>
  );
}
