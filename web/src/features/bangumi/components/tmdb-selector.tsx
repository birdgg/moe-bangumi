import { useState, useEffect, useRef } from "react";
import { useQuery } from "@tanstack/react-query";
import { AnimatePresence, motion } from "framer-motion";
import { createPortal } from "react-dom";
import { getApiBangumiSearchTmdbOptions } from "@/client/@tanstack/react-query.gen";
import type { BangumiResponse, TmdbSearchResult } from "@/client/types.gen";
import { Input } from "@/components/ui/input";
import { Spinner } from "@/components/ui/spinner";
import { useDebounce } from "@/hooks/use-debounce";
import {
  IconMovie,
  IconSearch,
  IconX,
  IconCheck,
} from "@tabler/icons-react";
import { cn } from "@/lib/utils";

interface TmdbPanelProps {
  open: boolean;
  onClose: () => void;
  bangumi: BangumiResponse;
  selectedTmdbId?: number;
  onSelect: (tmdbId: number) => void;
}

export function TmdbPanel({
  open,
  onClose,
  bangumi,
  selectedTmdbId,
  onSelect,
}: TmdbPanelProps) {
  const [keyword, setKeyword] = useState(bangumi.titleChs);
  const debouncedKeyword = useDebounce(keyword, 400);
  const inputRef = useRef<HTMLInputElement>(null);
  const year = new Date(bangumi.airDate).getFullYear();

  const {
    data: results,
    isLoading,
    isError,
  } = useQuery({
    ...getApiBangumiSearchTmdbOptions({
      query: { keyword: debouncedKeyword, year },
    }),
    enabled: open && debouncedKeyword.trim().length > 0,
  });

  const handleSelect = (tmdbId: number) => {
    onSelect(tmdbId);
    onClose();
  };

  const isSelected = (tmdbId: number) => selectedTmdbId === tmdbId;

  useEffect(() => {
    if (!open) return;
    const onKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    document.addEventListener("keydown", onKeyDown);
    return () => document.removeEventListener("keydown", onKeyDown);
  }, [open, onClose]);

  useEffect(() => {
    if (open) {
      requestAnimationFrame(() => inputRef.current?.focus());
    }
  }, [open]);

  return createPortal(
    <AnimatePresence>
      {open && (
        <>
          {/* Backdrop */}
          <motion.div
            className="fixed inset-0 z-[60] bg-black/30 dark:bg-black/50 backdrop-blur-sm"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            exit={{ opacity: 0 }}
            transition={{ duration: 0.2 }}
            onClick={onClose}
          />

          {/* Modal centering wrapper */}
          <div className="fixed inset-0 z-[60] flex items-center justify-center pointer-events-none">
            <motion.div
              className={cn(
                "pointer-events-auto",
                "w-[calc(100%-2rem)] max-w-lg",
                "h-[75vh] overflow-hidden",
                "rounded-2xl",
                "bg-linear-to-br from-white/95 via-white/90 to-chart-1/10",
                "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-3/20",
                "border border-chart-1/30 dark:border-chart-3/30",
                "shadow-2xl shadow-chart-1/20 dark:shadow-chart-3/50",
                "backdrop-blur-xl",
                "flex flex-col",
                "outline-none"
              )}
              initial={{ opacity: 0, scale: 0.92 }}
              animate={{ opacity: 1, scale: 1 }}
              exit={{ opacity: 0, scale: 0.92 }}
              transition={{ type: "spring", damping: 28, stiffness: 380 }}
            >
              {/* Header */}
              <div className="flex items-center justify-between px-5 pt-5 pb-3">
                <div className="flex items-center gap-2.5">
                  <div className="size-7 rounded-lg bg-gradient-to-br from-chart-1/20 to-chart-3/20 flex items-center justify-center">
                    <IconMovie className="size-3.5 text-chart-1" />
                  </div>
                  <div>
                    <h3 className="text-sm font-bold tracking-wide">
                      TMDB Match
                    </h3>
                    <p className="text-[10px] text-muted-foreground/50 tracking-wide mt-0.5">
                      {bangumi.titleChs}
                    </p>
                  </div>
                </div>
                <button
                  type="button"
                  onClick={onClose}
                  className="size-7 rounded-lg flex items-center justify-center text-muted-foreground/50 hover:text-foreground hover:bg-muted/60 transition-colors"
                >
                  <IconX className="size-4" />
                </button>
              </div>

              {/* Search */}
              <div className="px-5 pb-4">
                <div className="relative">
                  <IconSearch className="absolute left-3 top-1/2 -translate-y-1/2 size-4 text-muted-foreground/40" />
                  <Input
                    ref={inputRef}
                    value={keyword}
                    onChange={(e) => setKeyword(e.target.value)}
                    className="pl-9 h-9 text-sm"
                    placeholder="Search TMDB..."
                  />
                </div>
              </div>

              {/* Divider */}
              <div className="h-px bg-gradient-to-r from-transparent via-chart-1/30 to-transparent" />

              {/* Results */}
              <div className="overflow-y-auto flex-1 p-3 min-h-0">
                {isLoading ? (
                  <div className="flex flex-col items-center justify-center gap-3 py-16 text-muted-foreground/50">
                    <Spinner className="size-5" />
                    <span className="text-xs tracking-widest uppercase">
                      Searching
                    </span>
                  </div>
                ) : isError ? (
                  <div className="flex flex-col items-center justify-center gap-2 py-16">
                    <p className="text-xs text-destructive/70 tracking-wide">
                      Search failed
                    </p>
                    <p className="text-[10px] text-muted-foreground/40">
                      Check TMDB API Key configuration
                    </p>
                  </div>
                ) : results && results.length > 0 ? (
                  <div className="space-y-1.5">
                    {results.map((result: TmdbSearchResult, i: number) => (
                      <motion.button
                        key={result.tmdbId}
                        type="button"
                        onClick={() => handleSelect(result.tmdbId)}
                        className={cn(
                          "group w-full flex items-center gap-4 px-3 py-2.5 rounded-xl text-left",
                          "hover:bg-muted/50 transition-all duration-200",
                          isSelected(result.tmdbId) &&
                          "bg-chart-1/8 ring-1 ring-chart-1/25"
                        )}
                        initial={{ opacity: 0, y: 10 }}
                        animate={{ opacity: 1, y: 0 }}
                        transition={{ delay: i * 0.04, duration: 0.3 }}
                      >
                        {/* Poster */}
                        {result.posterUrl ? (
                          <img
                            src={result.posterUrl}
                            alt={result.title}
                            className="w-11 h-16 object-cover rounded-lg shrink-0 shadow-sm"
                          />
                        ) : (
                          <div className="w-11 h-16 rounded-lg bg-muted/60 flex items-center justify-center shrink-0">
                            <IconMovie className="size-4 text-muted-foreground/25" />
                          </div>
                        )}

                        {/* Info */}
                        <div className="flex-1 min-w-0">
                          <p className="text-sm font-medium truncate leading-snug">
                            {result.title}
                          </p>
                          <div className="flex items-center gap-2 mt-1">
                            {result.year && (
                              <span className="text-[11px] text-muted-foreground/60 tabular-nums">
                                {result.year}
                              </span>
                            )}
                            <span className="text-[10px] px-1.5 py-px rounded-md bg-muted/60 text-muted-foreground/50 uppercase tracking-wider font-medium">
                              {result.mediaType}
                            </span>
                            <span className="text-[10px] text-muted-foreground/30 tabular-nums">
                              #{result.tmdbId}
                            </span>
                          </div>
                        </div>

                        {/* Selection indicator */}
                        {isSelected(result.tmdbId) ? (
                          <div className="size-6 rounded-full bg-chart-1 flex items-center justify-center shrink-0 shadow-sm shadow-chart-1/30">
                            <IconCheck className="size-3.5 text-white" />
                          </div>
                        ) : null
                        }
                      </motion.button>
                    ))}
                  </div>
                ) : debouncedKeyword.trim() ? (
                  <div className="flex flex-col items-center justify-center gap-2 py-16">
                    <IconSearch className="size-5 text-muted-foreground/20" />
                    <p className="text-xs text-muted-foreground/40 tracking-wide">
                      No matching TMDB results
                    </p>
                  </div>
                ) : (
                  <div className="flex flex-col items-center justify-center gap-2 py-16">
                    <IconSearch className="size-5 text-muted-foreground/15" />
                    <p className="text-xs text-muted-foreground/30 tracking-wide">
                      Type to search
                    </p>
                  </div>
                )}
              </div>
            </motion.div>
          </div>
        </>
      )}
    </AnimatePresence>,
    document.body
  );
}
