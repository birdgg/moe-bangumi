import { useState, useEffect, useRef } from "react";
import { useQuery } from "@tanstack/react-query";
import { createPortal } from "react-dom";
import { getApiBangumiSearchMikanOptions } from "@/client/@tanstack/react-query.gen";
import type { BangumiResponse, MikanSearchResultDto } from "@/client/types.gen";
import { Input } from "@/components/ui/input";
import { Spinner } from "@/components/ui/spinner";
import { useDebounce } from "@/hooks/use-debounce";
import { IconSearch, IconX, IconCheck } from "@tabler/icons-react";
import { cn } from "@/lib/utils";

interface MikanPanelProps {
  open: boolean;
  onClose: () => void;
  bangumi: BangumiResponse;
  onSelect: (mikanId: number) => void;
}

export function MikanPanel({
  open,
  onClose,
  bangumi,
  onSelect,
}: MikanPanelProps) {
  const [keyword, setKeyword] = useState(bangumi.titleChs);
  const debouncedKeyword = useDebounce(keyword, 400);
  const inputRef = useRef<HTMLInputElement>(null);
  const [selectedId, setSelectedId] = useState<number | null>(null);

  const {
    data: results,
    isLoading,
    isError,
  } = useQuery({
    ...getApiBangumiSearchMikanOptions({
      query: { keyword: debouncedKeyword },
    }),
    enabled: open && debouncedKeyword.trim().length > 0,
  });

  const handleSelect = (mikanId: number) => {
    setSelectedId(mikanId);
    onSelect(mikanId);
    onClose();
  };

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

  if (!open) return null;

  return createPortal(
    <>
      <div
        className="fixed inset-0 z-[60] bg-black/30 dark:bg-black/50 backdrop-blur-sm animate-in fade-in duration-150"
        onClick={onClose}
      />

      <div className="fixed inset-0 z-[60] flex items-center justify-center pointer-events-none">
        <div
          className={cn(
            "pointer-events-auto",
            "w-[calc(100%-2rem)] max-w-lg",
            "max-h-[60vh] overflow-hidden",
            "rounded-2xl",
            "bg-linear-to-br from-white/95 via-white/90 to-chart-3/10",
            "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-1/20",
            "border border-chart-3/30 dark:border-chart-1/30",
            "shadow-2xl shadow-chart-3/20 dark:shadow-chart-1/50",
            "backdrop-blur-xl",
            "flex flex-col",
            "outline-hidden",
            "animate-modal-popup-in"
          )}
        >
          {/* Header */}
          <div className="flex items-center justify-between px-5 pt-5 pb-3">
            <div className="flex items-center gap-2.5">
              <div className="size-7 rounded-lg bg-linear-to-br from-chart-3/20 to-chart-1/20 flex items-center justify-center">
                <IconSearch className="size-3.5 text-chart-3" />
              </div>
              <div>
                <h3 className="text-sm font-bold tracking-wide">
                  Mikan Search
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
                placeholder="Search Mikan..."
              />
            </div>
          </div>

          {/* Divider */}
          <div className="h-px bg-linear-to-r from-transparent via-chart-3/30 to-transparent" />

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
              </div>
            ) : results && results.length > 0 ? (
              <div className="space-y-1.5">
                {results.map((result: MikanSearchResultDto) => (
                  <button
                    key={result.mikanId}
                    type="button"
                    onClick={() => handleSelect(result.mikanId)}
                    className={cn(
                      "group w-full flex items-center gap-4 px-3 py-2.5 rounded-xl text-left",
                      "hover:bg-muted/50 transition-colors duration-150",
                      selectedId === result.mikanId &&
                        "bg-chart-3/8 ring-1 ring-chart-3/25"
                    )}
                  >
                    <div className="flex-1 min-w-0">
                      <p className="text-sm font-medium truncate leading-snug">
                        {result.title}
                      </p>
                      <div className="flex items-center gap-2 mt-1">
                        {result.season && (
                          <span className="text-[10px] px-1.5 py-px rounded-md bg-chart-1/10 text-chart-1 font-semibold tracking-wider">
                            S{result.season}
                          </span>
                        )}
                        <span className="text-[10px] text-muted-foreground/30 tabular-nums">
                          #{result.mikanId}
                        </span>
                      </div>
                    </div>

                    {selectedId === result.mikanId && (
                      <div className="size-6 rounded-full bg-chart-3 flex items-center justify-center shrink-0 shadow-xs shadow-chart-3/30">
                        <IconCheck className="size-3.5 text-white" />
                      </div>
                    )}
                  </button>
                ))}
              </div>
            ) : debouncedKeyword.trim() ? (
              <div className="flex flex-col items-center justify-center gap-2 py-16">
                <IconSearch className="size-5 text-muted-foreground/20" />
                <p className="text-xs text-muted-foreground/40 tracking-wide">
                  No matching Mikan results
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
        </div>
      </div>
    </>,
    document.body
  );
}
