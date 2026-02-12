import { useCallback, useState } from "react";
import { motion } from "framer-motion";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import {
  deleteApiTrackingByIdMutation,
  getApiTrackingBangumisQueryKey,
  getApiTrackingOptions,
  getApiTrackingQueryKey,
  postApiTrackingMutation,
} from "@/client/@tanstack/react-query.gen";
import type { BangumiResponse } from "@/client/types.gen";
import { toast } from "sonner";

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
      queryClient.invalidateQueries({ queryKey: getApiTrackingQueryKey() });
      queryClient.invalidateQueries({ queryKey: getApiTrackingBangumisQueryKey() });
      toast.success("订阅成功");
    },
    onError: () => {
      toast.error("订阅失败");
    },
  });

  const unsubscribeMutation = useMutation({
    ...deleteApiTrackingByIdMutation(),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: getApiTrackingQueryKey() });
      queryClient.invalidateQueries({ queryKey: getApiTrackingBangumisQueryKey() });
      toast.success("取消订阅成功");
    },
    onError: () => {
      toast.error("取消订阅失败");
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
          trackingType: "subscription",
        },
      });
    }
  };

  const seasonText =
    bangumi.kind === "tv" && bangumi.season && bangumi.season > 1
      ? ` 第${bangumi.season}季`
      : "";

  return (
    <motion.div
      className="group relative w-[13rem]"
      whileHover={{ y: -6 }}
      whileTap={{ scale: 0.97 }}
      transition={{ type: "spring", stiffness: 300, damping: 22 }}
    >
      <div className="poster-card relative aspect-[2/3] overflow-hidden rounded-xl">
        {bangumi.posterUrl ? (
          <PosterImage url={bangumi.posterUrl} alt={bangumi.titleChs} />
        ) : (
          <div className="flex size-full items-center justify-center bg-gradient-to-br from-muted to-muted/50">
            <span className="text-muted-foreground text-xs">No Image</span>
          </div>
        )}

        {/* Cinematic gradient scrim */}
        <div className="absolute inset-x-0 bottom-0 h-3/5 bg-gradient-to-t from-black/85 via-black/40 to-transparent pointer-events-none" />

        {/* Hover vignette */}
        <div className="absolute inset-0 bg-black/0 group-hover:bg-black/10 transition-colors duration-300 pointer-events-none" />

        {/* Subscribed badge */}
        {isSubscribed && (
          <div className="absolute top-2 right-2 z-10">
            <span className="glass-badge text-xs">已订阅</span>
          </div>
        )}

        {/* Subscribe button */}
        {!isSubscribed && bangumi.mikanId && (
          <div className="absolute top-2 right-2 z-10">
            <button
              type="button"
              onClick={handleSubscribe}
              disabled={isPending}
              className="subscribe-btn-corner opacity-0 group-hover:opacity-100 transition-all duration-300"
            >
              {isPending ? (
                <svg
                  className="size-3 animate-spin"
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
              ) : (
                <span>订阅</span>
              )}
            </button>
          </div>
        )}

        {/* Title */}
        <div className="absolute inset-x-0 bottom-0 p-3 pb-3.5">
          <h3 className="line-clamp-2 text-xs font-semibold leading-snug text-white drop-shadow-[0_1px_3px_rgba(0,0,0,0.6)]">
            {bangumi.titleChs}
            {seasonText && (
              <span className="text-white/50 font-normal">{seasonText}</span>
            )}
          </h3>
        </div>
      </div>
    </motion.div>
  );
}

function PosterImage({ url, alt }: { url: string; alt: string }) {
  const [loaded, setLoaded] = useState(false);
  const onLoad = useCallback(() => setLoaded(true), []);

  return (
    <img
      src={url}
      alt={alt}
      loading="lazy"
      decoding="async"
      onLoad={onLoad}
      className={`size-full object-cover transition-all duration-500 ease-out group-hover:scale-[1.06] ${
        loaded ? "opacity-100" : "opacity-0"
      }`}
    />
  );
}
