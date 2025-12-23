import * as React from "react";
import { cn } from "@/lib/utils";
import { useSearchTmdb } from "../hooks/use-bangumi";
import { type TvShow } from "@/lib/api";
import {
  Combobox,
  ComboboxInput,
  ComboboxContent,
  ComboboxList,
  ComboboxItem,
  ComboboxEmpty,
} from "@/components/ui/combobox";
import { IconLoader2, IconMovie, IconCheck } from "@tabler/icons-react";
import { useDebouncedValue } from "@tanstack/react-pacer";

interface TmdbMatcherProps {
  value: TvShow | null;
  onChange: (show: TvShow | null) => void;
  initialKeyword?: string;
  className?: string;
}

const TMDB_IMAGE_BASE = "https://image.tmdb.org/t/p/w92";

export function TmdbMatcher({
  value,
  onChange,
  initialKeyword = "",
  className,
}: TmdbMatcherProps) {
  const [inputValue, setInputValue] = React.useState(initialKeyword);
  const [debouncedKeyword] = useDebouncedValue(inputValue, { wait: 400 });
  const [open, setOpen] = React.useState(false);
  const hasSearchedRef = React.useRef(false);

  const { data, isLoading, isFetching } = useSearchTmdb(debouncedKeyword);
  const results = data ?? [];
  const isSearching = isLoading || isFetching;

  // Build a map for quick lookup
  const resultsMap = React.useMemo(() => {
    const map = new Map<string, TvShow>();
    for (const show of results) {
      map.set(show.id.toString(), show);
    }
    return map;
  }, [results]);

  // Auto-select first result when search completes (only once per search term)
  React.useEffect(() => {
    if (
      !isSearching &&
      results.length > 0 &&
      debouncedKeyword &&
      !hasSearchedRef.current
    ) {
      hasSearchedRef.current = true;
      onChange(results[0]);
    }
  }, [isSearching, results, debouncedKeyword, onChange]);

  // Reset search flag when keyword changes
  React.useEffect(() => {
    hasSearchedRef.current = false;
  }, [debouncedKeyword]);

  // Update input when initialKeyword changes
  React.useEffect(() => {
    if (initialKeyword && initialKeyword !== inputValue) {
      setInputValue(initialKeyword);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [initialKeyword]);

  const handleValueChange = (newValue: string | null) => {
    if (newValue) {
      const selected = resultsMap.get(newValue);
      if (selected) {
        onChange(selected);
        setOpen(false);
      }
    } else {
      onChange(null);
    }
  };

  return (
    <div className={cn("space-y-2", className)}>
      <Combobox
        open={open}
        onOpenChange={setOpen}
        value={value?.id?.toString() ?? null}
        onValueChange={handleValueChange}
      >
        <div className="relative">
          <ComboboxInput
            value={inputValue}
            onChange={(e) => setInputValue(e.target.value)}
            placeholder="搜索 TMDB..."
            className="w-full"
            showClear={!!inputValue}
          />
          {isSearching && (
            <div className="absolute right-10 top-1/2 -translate-y-1/2">
              <IconLoader2 className="size-4 animate-spin text-chart-3 dark:text-chart-1" />
            </div>
          )}
        </div>

        <ComboboxContent>
          <ComboboxList>
            <ComboboxEmpty>
              {isSearching ? (
                <span className="flex items-center gap-2">
                  <IconLoader2 className="size-4 animate-spin" />
                  搜索中...
                </span>
              ) : inputValue ? (
                "未找到结果"
              ) : (
                "输入关键词搜索"
              )}
            </ComboboxEmpty>
            {results.map((show: TvShow) => (
              <ComboboxItem
                key={show.id}
                value={show.id.toString()}
                className="flex items-center gap-3 py-2"
              >
                {/* Poster thumbnail */}
                <div className="relative size-10 shrink-0 overflow-hidden rounded-md bg-muted">
                  {show.poster_path ? (
                    <img
                      src={`${TMDB_IMAGE_BASE}${show.poster_path}`}
                      alt={show.name}
                      className="size-full object-cover"
                    />
                  ) : (
                    <div className="flex size-full items-center justify-center">
                      <IconMovie className="size-5 text-muted-foreground" />
                    </div>
                  )}
                </div>

                {/* Show info */}
                <div className="flex min-w-0 flex-1 flex-col">
                  <span className="truncate font-medium">{show.name}</span>
                  <div className="flex items-center gap-2 text-xs text-muted-foreground">
                    <span className="font-mono text-chart-3 dark:text-chart-1">
                      #{show.id}
                    </span>
                    {show.first_air_date && (
                      <>
                        <span className="text-border">|</span>
                        <span>{show.first_air_date.split("-")[0]}</span>
                      </>
                    )}
                    {show.original_name !== show.name && (
                      <>
                        <span className="text-border">|</span>
                        <span className="truncate">{show.original_name}</span>
                      </>
                    )}
                  </div>
                </div>
              </ComboboxItem>
            ))}
          </ComboboxList>
        </ComboboxContent>
      </Combobox>

      {/* Selected item preview - simple display */}
      {value && !open && (
        <div className="flex items-center gap-2 text-sm text-muted-foreground animate-in fade-in-0 duration-200">
          <IconCheck className="size-4 shrink-0 text-chart-3 dark:text-chart-1" />
          <span className="truncate">{value.name}</span>
          <span className="font-mono text-xs text-chart-3 dark:text-chart-1">
            #{value.id}
          </span>
        </div>
      )}
    </div>
  );
}
