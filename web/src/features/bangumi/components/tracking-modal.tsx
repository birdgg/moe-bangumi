import { useState, useCallback } from "react";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import {
  postApiTrackingMutation,
  putApiTrackingByIdMutation,
  putApiBangumiByIdTmdbIdMutation,
  getApiTrackingQueryKey,
  getApiTrackingBangumisQueryKey,
} from "@/client/@tanstack/react-query.gen";

import type { BangumiResponse, TrackingResponse } from "@/client/types.gen";
import {
  AnimatedModal,
  AnimatedModalTitle,
} from "@/components/ui/animated-modal";
import { Input } from "@/components/ui/input";
import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import { Spinner } from "@/components/ui/spinner";
import { toast } from "sonner";
import { Switch } from "@/components/ui/switch";
import { IconRss, IconHash, IconMovie, IconSearch } from "@tabler/icons-react";
import { TmdbPanel } from "./tmdb-selector";

interface TrackingModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  bangumi: BangumiResponse;
  /** Pass existing tracking for edit mode */
  tracking?: TrackingResponse;
}

export function TrackingModal({
  open,
  onOpenChange,
  bangumi,
  tracking,
}: TrackingModalProps) {
  const [tmdbOpen, setTmdbOpen] = useState(false);
  const [tmdbId, setTmdbId] = useState(bangumi.tmdbId);

  const handleOpenChange = (newOpen: boolean) => {
    if (!newOpen) setTmdbOpen(false);
    onOpenChange(newOpen);
  };

  return (
    <>
      <AnimatedModal
        open={open}
        onOpenChange={handleOpenChange}
        className="max-w-md"
      >
        <TrackingForm
          key={`${bangumi.id}-${tracking?.id ?? "new"}`}
          bangumi={bangumi}
          tracking={tracking}
          tmdbId={tmdbId}
          onClose={() => handleOpenChange(false)}
          onOpenTmdb={() => setTmdbOpen(true)}
        />
      </AnimatedModal>
      <TmdbPanel
        open={open && tmdbOpen}
        onClose={() => setTmdbOpen(false)}
        bangumi={bangumi}
        selectedTmdbId={tmdbId}
        onSelect={setTmdbId}
      />
    </>
  );
}

function TrackingForm({
  bangumi,
  tracking,
  tmdbId,
  onClose,
  onOpenTmdb,
}: {
  bangumi: BangumiResponse;
  tracking?: TrackingResponse;
  tmdbId?: number;
  onClose: () => void;
  onOpenTmdb: () => void;
}) {
  const isEditing = !!tracking;
  const queryClient = useQueryClient();

  const [episodeOffset, setEpisodeOffset] = useState(
    String(tracking?.episodeOffset ?? 0)
  );
  const [rssUrl, setRssUrl] = useState(tracking?.rssUrl ?? "");
  const [autoComplete, setAutoComplete] = useState(
    tracking?.autoComplete ?? true
  );
  const tmdbChanged = tmdbId !== bangumi.tmdbId;

  const invalidateTracking = useCallback(() => {
    queryClient.invalidateQueries({ queryKey: getApiTrackingQueryKey() });
    queryClient.invalidateQueries({
      queryKey: getApiTrackingBangumisQueryKey(),
    });
  }, [queryClient]);

  const createMutation = useMutation({
    ...postApiTrackingMutation(),
    onSuccess: () => {
      invalidateTracking();
      toast.success("追番成功");
      onClose();
    },
    onError: () => toast.error("追番失败"),
  });

  const updateMutation = useMutation({
    ...putApiTrackingByIdMutation(),
    onSuccess: () => {
      invalidateTracking();
      toast.success("更新成功");
      onClose();
    },
    onError: () => toast.error("更新失败"),
  });

  const tmdbMutation = useMutation({
    ...putApiBangumiByIdTmdbIdMutation(),
  });

  const isPending =
    createMutation.isPending ||
    updateMutation.isPending ||
    tmdbMutation.isPending;

  const handleSubmit = () => {
    const ep = parseInt(episodeOffset, 10) || 0;

    if (tmdbChanged) {
      tmdbMutation.mutate({
        path: { id: bangumi.id },
        body: { tmdbId: tmdbId },
      });
    }

    if (isEditing && tracking) {
      updateMutation.mutate({
        path: { id: tracking.id },
        body: {
          episodeOffset: ep,
          rssUrl: rssUrl.trim() || undefined,
          autoComplete,
        },
      });
    } else {
      createMutation.mutate({
        body: {
          bangumiId: bangumi.id,
          trackingType: "subscription",
          mikanId: bangumi.mikanId,
          rssUrl: rssUrl.trim() || undefined,
          episodeOffset: ep || undefined,
        },
      });
    }
  };

  const seasonText =
    bangumi.kind === "tv" && bangumi.season && bangumi.season > 1
      ? `第${bangumi.season}季`
      : null;

  return (
    <div className="flex flex-col overflow-hidden">
      {/* Header: Cinematic poster banner */}
      <div className="relative overflow-hidden">
        {/* Blurred poster background */}
        {bangumi.posterUrl && (
          <div className="absolute inset-0">
            <img
              src={bangumi.posterUrl}
              alt=""
              className="size-full object-cover scale-110 blur-xl opacity-30 dark:opacity-20"
            />
            <div className="absolute inset-0 bg-linear-to-b from-background/60 via-background/80 to-background" />
          </div>
        )}

        <div className="relative flex gap-3 p-3 sm:p-4">
          {/* Poster thumbnail */}
          {bangumi.posterUrl ? (
            <img
              src={bangumi.posterUrl}
              alt={bangumi.titleChs}
              className="poster-card w-16 sm:w-18 aspect-[2/3] object-cover rounded-lg shrink-0"
            />
          ) : (
            <div className="w-16 sm:w-18 aspect-[2/3] rounded-lg shrink-0 bg-linear-to-br from-chart-3/20 via-chart-1/10 to-chart-5/15 flex items-center justify-center border border-border/50">
              <IconMovie className="size-6 text-muted-foreground/20" />
            </div>
          )}

          {/* Info */}
          <div className="flex flex-col justify-between min-w-0 py-0.5">
            <div>
              <AnimatedModalTitle className="text-sm font-bold leading-snug line-clamp-2">
                {bangumi.titleChs}
              </AnimatedModalTitle>

              {bangumi.titleJap && (
                <p className="text-[10px] text-muted-foreground/40 mt-0.5 line-clamp-1">
                  {bangumi.titleJap}
                </p>
              )}
            </div>

            <div className="flex flex-wrap items-center gap-1.5 mt-1.5">
              {seasonText && (
                <span className="inline-flex items-center px-1.5 py-px rounded-md bg-chart-1/10 text-chart-1 text-[10px] font-semibold tracking-wider">
                  {seasonText}
                </span>
              )}
              <span className="inline-flex items-center px-1.5 py-px rounded-md bg-muted text-muted-foreground text-[10px] font-medium uppercase tracking-wider">
                {bangumi.kind}
              </span>
              <span className="inline-flex items-center px-1.5 py-px rounded-md bg-muted text-muted-foreground text-[10px] font-medium tracking-wider">
                {bangumi.airDate}
              </span>
            </div>
          </div>
        </div>
      </div>

      {/* Accent gradient line */}
      <div className="h-px bg-linear-to-r from-transparent via-chart-1/30 to-transparent" />

      {/* Form fields */}
      <div className="p-4 sm:p-5 space-y-3.5 overflow-y-auto max-h-[50vh]">
        {/* Episode offset */}
        <div>
          <Label
            htmlFor="ep-offset"
            className="text-[10px] tracking-widest uppercase text-muted-foreground/60 mb-1.5"
          >
            <IconHash className="size-3" />
            集数偏移量
          </Label>
          <Input
            id="ep-offset"
            type="number"
            min={0}
            value={episodeOffset}
            onChange={(e) => setEpisodeOffset(e.target.value)}
            placeholder="0"
          />
          <p className="text-[10px] text-muted-foreground/40 mt-1 tracking-wide">
            文件命名时的集数起始偏移量
          </p>
        </div>

        {/* TMDB Selector trigger */}
        <div>
          <Label className="text-[10px] tracking-widest uppercase text-muted-foreground/60 mb-1.5">
            <IconMovie className="size-3" />
            TMDB
          </Label>
          <button
            type="button"
            onClick={onOpenTmdb}
            className="w-full flex items-center justify-between h-8 px-2.5 rounded-lg border border-input text-sm hover:bg-muted/50 transition-colors"
          >
            <span
              className={
                tmdbId
                  ? "text-foreground"
                  : "text-muted-foreground/50"
              }
            >
              {tmdbId
                ? tmdbId
                : "选择 TMDB 匹配"}
            </span>
            <IconSearch className="size-3.5 text-muted-foreground/40" />
          </button>
        </div>

        {/* RSS URL */}
        <div>
          <Label
            htmlFor="rss-url"
            className="text-[10px] tracking-widest uppercase text-muted-foreground/60 mb-1.5"
          >
            <IconRss className="size-3" />
            RSS Feed
          </Label>
          <Input
            id="rss-url"
            type="url"
            value={rssUrl}
            onChange={(e) => setRssUrl(e.target.value)}
            placeholder={
              bangumi.mikanId
                ? "留空则自动使用 Mikan 订阅"
                : "自定义 RSS 订阅地址"
            }
          />
          {bangumi.mikanId && !rssUrl && (
            <p className="flex items-center gap-1.5 text-[10px] text-chart-1/60 mt-1 tracking-wide">
              <span className="size-1 rounded-full bg-chart-1/50 animate-pulse" />
              将自动使用 Mikan RSS 订阅
            </p>
          )}
        </div>

        {/* Auto Complete */}
        <div className="flex items-center justify-between">
          <div className="space-y-0.5">
            <Label className="text-[10px] tracking-widest uppercase text-muted-foreground/60">
              自动补全
            </Label>
            <p className="text-[10px] text-muted-foreground/40 tracking-wide">
              关闭后仅下载最新一集
            </p>
          </div>
          <Switch
            checked={autoComplete}
            onCheckedChange={setAutoComplete}
          />
        </div>
      </div>

      {/* Footer */}
      <div className="h-px bg-linear-to-r from-transparent via-border/40 to-transparent" />
      <div className="flex items-center justify-end gap-2 px-4 py-3 sm:px-5">
        <Button
          size="sm"
          disabled={isPending}
          onClick={handleSubmit}
          className="bg-linear-to-r from-chart-1 to-chart-3 text-white border-chart-1/20 shadow-lg shadow-chart-1/15 hover:shadow-chart-1/25 hover:brightness-110 transition-all"
        >
          {isPending ? (
            <>
              <Spinner className="size-3.5" />
              <span>{isEditing ? "保存中..." : "追番中..."}</span>
            </>
          ) : (
            <span>{isEditing ? "保存" : "追番"}</span>
          )}
        </Button>
      </div>
    </div>
  );
}
