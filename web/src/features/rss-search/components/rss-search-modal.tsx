import { useState, useEffect, useCallback } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { Command } from "cmdk";
import { toast } from "sonner";
import { Spinner } from "@/components/ui/spinner";
import { IconSearch } from "@tabler/icons-react";
import {
  getApiRssSearchOptions,
  postApiRssDownloadMutation,
} from "@/client/@tanstack/react-query.gen";
import type { RssSearchResult } from "@/client/types.gen";

interface RssSearchModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
}

function useDebounce(value: string, delay: number): string {
  const [debounced, setDebounced] = useState(value);

  useEffect(() => {
    const timer = setTimeout(() => setDebounced(value), delay);
    return () => clearTimeout(timer);
  }, [value, delay]);

  return debounced;
}

const SOURCE_COLORS: Record<string, { tint: string; text: string }> = {
  Nyaa: { tint: "bg-chart-1/10", text: "text-chart-1" },
  AcgRip: { tint: "bg-chart-3/10", text: "text-chart-3" },
};

function getSourceColor(source: string) {
  return (
    SOURCE_COLORS[source] ?? {
      tint: "bg-foreground/5",
      text: "text-muted-foreground",
    }
  );
}

function ResultItem({
  item,
  onDownload,
}: {
  item: RssSearchResult;
  onDownload: (url: string) => void;
}) {
  const color = getSourceColor(item.source);

  return (
    <Command.Item
      value={`${item.source}-${item.title}`}
      onSelect={() => {
        if (item.torrentUrl) onDownload(item.torrentUrl);
      }}
      className="glass-search-item group/item relative flex items-start gap-3 rounded-xl px-3 py-2.5 cursor-default select-none transition-all duration-150"
    >
      <div className="min-w-0 flex-1">
        <p className="text-[13px] leading-relaxed text-foreground/75 break-all group-data-[selected=true]/item:text-foreground transition-colors">
          {item.title}
        </p>

        <div className="flex items-center gap-2 mt-1.5">
          <span
            className={`glass-pill inline-flex items-center rounded-md px-1.5 py-0.5 text-[10px] font-semibold tracking-wide uppercase ${color.tint} ${color.text}`}
          >
            {item.source}
          </span>
          {item.pubDate && (
            <span className="text-[10px] text-muted-foreground/40">
              {new Date(item.pubDate).toLocaleDateString("zh-CN", {
                year: "numeric",
                month: "2-digit",
                day: "2-digit",
              })}
            </span>
          )}
        </div>
      </div>
    </Command.Item>
  );
}

const FILTER_TAGS = ["BDRip", "VCB-Studio"] as const;

export function RssSearchModal({ open, onOpenChange }: RssSearchModalProps) {
  const [keyword, setKeyword] = useState("");
  const [selectedTags, setSelectedTags] = useState<Set<string>>(
    () => new Set(FILTER_TAGS),
  );
  const debouncedKeyword = useDebounce(keyword, 800);

  const searchKeyword =
    selectedTags.size > 0
      ? [debouncedKeyword, ...selectedTags].join(" ").trim()
      : debouncedKeyword;

  const {
    data: results,
    isLoading,
    isFetching,
  } = useQuery({
    ...getApiRssSearchOptions({
      query: { keyword: searchKeyword },
    }),
    enabled: debouncedKeyword.length > 0,
  });

  const downloadMutation = useMutation({
    ...postApiRssDownloadMutation(),
    onSuccess: () => {
      toast.success("已发送到下载器");
    },
    onError: () => {
      toast.error("发送到下载器失败");
    },
  });

  const handleDownload = useCallback(
    (torrentUrl: string) => {
      downloadMutation.mutate({ body: { torrentUrl } });
    },
    [downloadMutation.mutate],
  );

  const toggleTag = useCallback((tag: string) => {
    setSelectedTags((prev) => {
      const next = new Set(prev);
      if (next.has(tag)) {
        next.delete(tag);
      } else {
        next.add(tag);
      }
      return next;
    });
  }, []);

  const handleOpenChange = useCallback(
    (next: boolean) => {
      onOpenChange(next);
      if (!next) {
        setKeyword("");
        setSelectedTags(new Set(FILTER_TAGS));
      }
    },
    [onOpenChange],
  );

  const showSpinner = isLoading || isFetching;
  const hasResults = results && results.length > 0;

  return (
    <Command.Dialog
      open={open}
      onOpenChange={handleOpenChange}
      shouldFilter={false}
      loop
      label="RSS Search"
      overlayClassName="fixed inset-0 z-50 bg-black/10 dark:bg-black/30 backdrop-blur-sm"
      contentClassName={[
        "fixed left-1/2 top-[45%] z-50 -translate-y-1/2 -translate-x-1/2",
        "w-[calc(100%-2rem)] max-w-xl",
        "max-h-[90vh] overflow-hidden",
        "rounded-2xl",
        "glass-search",
        "outline-hidden",
      ].join(" ")}
    >
      <div className="relative flex flex-col max-h-[70vh]">
        {/* Refraction highlight */}
        <div className="absolute inset-x-0 top-0 h-24 rounded-t-2xl pointer-events-none bg-[radial-gradient(ellipse_80%_50%_at_50%_-20%,oklch(1_0_0_/_0.18),transparent_70%)] dark:bg-[radial-gradient(ellipse_80%_50%_at_50%_-20%,oklch(1_0_0_/_0.05),transparent_70%)]" />

        {/* Input */}
        <div className="flex items-center gap-3 px-4 py-3.5">
          <IconSearch className="size-[18px] shrink-0 text-foreground/30" />
          <Command.Input
            placeholder="Search Nyaa, AcgRip..."
            value={keyword}
            onValueChange={setKeyword}
            className="flex-1 bg-transparent text-sm text-foreground placeholder:text-foreground/25 outline-hidden"
            autoFocus
          />
          {isFetching && debouncedKeyword.length > 0 && (
            <Spinner className="size-4 text-foreground/30" />
          )}
        </div>

        {/* Filter tags */}
        <div className="flex items-center gap-1.5 px-4 pb-2.5">
          {FILTER_TAGS.map((tag) => {
            const active = selectedTags.has(tag);
            return (
              <button
                key={tag}
                type="button"
                onClick={() => toggleTag(tag)}
                className={[
                  "glass-pill rounded-md px-2 py-0.5 text-[11px] font-medium transition-all duration-150",
                  active
                    ? "bg-chart-2/12 text-chart-2"
                    : "bg-foreground/[0.03] text-foreground/30 hover:bg-foreground/[0.06] hover:text-foreground/50",
                ].join(" ")}
              >
                {tag}
              </button>
            );
          })}
          {hasResults && (
            <span className="ml-auto text-[10px] text-foreground/25 font-medium tabular-nums">
              {results.length}
            </span>
          )}
        </div>

        {/* Divider */}
        <div className="relative h-px mx-4">
          <div className="absolute inset-0 bg-foreground/[0.06]" />
          <div className="absolute inset-0 bg-linear-to-r from-transparent via-foreground/10 to-transparent" />
        </div>

        {/* Results */}
        <Command.List className="no-scrollbar flex-1 overflow-y-auto px-1.5 py-1.5 scroll-smooth max-h-[50vh]">
          {showSpinner && (
            <Command.Loading className="flex items-center justify-center py-14">
              <Spinner className="size-5 text-foreground/25" />
            </Command.Loading>
          )}

          {!showSpinner && debouncedKeyword.length > 0 && (
            <Command.Empty className="flex flex-col items-center justify-center py-14 text-center">
              <p className="text-xs text-foreground/25">
                No results for &ldquo;{debouncedKeyword}&rdquo;
              </p>
            </Command.Empty>
          )}

          {hasResults && (
            <Command.Group>
              {results.map((item, index) => (
                <ResultItem
                  key={`${item.source}-${index}`}
                  item={item}
                  onDownload={handleDownload}
                />
              ))}
            </Command.Group>
          )}

          {!showSpinner && debouncedKeyword.length === 0 && (
            <div className="flex flex-col items-center justify-center py-14 text-center">
              <div className="flex items-center gap-3 mb-3">
                <span className="glass-pill inline-flex items-center rounded-lg bg-chart-1/8 px-2 py-1 text-[10px] font-semibold text-chart-1 tracking-wide uppercase">
                  Nyaa
                </span>
                <span className="text-foreground/15 text-xs">&</span>
                <span className="glass-pill inline-flex items-center rounded-lg bg-chart-3/8 px-2 py-1 text-[10px] font-semibold text-chart-3 tracking-wide uppercase">
                  AcgRip
                </span>
              </div>
              <p className="text-xs text-foreground/25">
                Type to search across RSS sources
              </p>
            </div>
          )}
        </Command.List>

        {/* Footer keyboard hints */}
        {hasResults && (
          <div className="flex items-center gap-3 px-4 py-2 border-t border-foreground/[0.06]">
            <div className="flex items-center gap-1.5 text-[10px] text-foreground/30">
              <kbd className="glass-kbd rounded px-1 py-0.5 font-mono text-[9px]">
                &uarr;&darr;
              </kbd>
              <span>导航</span>
            </div>
            <div className="flex items-center gap-1.5 text-[10px] text-foreground/30">
              <kbd className="glass-kbd rounded px-1 py-0.5 font-mono text-[9px]">
                &crarr;
              </kbd>
              <span>下载</span>
            </div>
            <div className="flex items-center gap-1.5 text-[10px] text-foreground/30">
              <kbd className="glass-kbd rounded px-1 py-0.5 font-mono text-[9px]">
                esc
              </kbd>
              <span>关闭</span>
            </div>
          </div>
        )}
      </div>
    </Command.Dialog>
  );
}
