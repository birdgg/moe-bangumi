import { useState, useEffect, useRef } from "react";
import { useMutation, useQuery } from "@tanstack/react-query";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { motion, AnimatePresence } from "framer-motion";
import { toast } from "sonner";
import { cn } from "@/lib/utils";
import {
  getApiTorrentSearchOptions,
  postApiCollectionAddMutation,
  postApiCollectionConfirmMutation,
  deleteApiCollectionByHashMutation,
} from "@/client/@tanstack/react-query.gen";
import { getApiCollectionFilesByHash } from "@/client/sdk.gen";
import type { FileTreeNode } from "./file-tree-utils";
import { buildFileTree, findQuickToggleFolders, findQuickToggleContent } from "./file-tree-utils";
import { SearchPanel } from "./search-panel";
import { FilePanel, type PreviewState } from "./file-panel";

interface TorrentSearchModalProps {
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

const MAX_POLLS = 10;

export function TorrentSearchModal({ open, onOpenChange }: TorrentSearchModalProps) {
  // Search state
  const [keyword, setKeyword] = useState("");
  const debouncedKeyword = useDebounce(keyword, 800);
  const searchKeyword = [debouncedKeyword, "BDRip", "VCB-Studio"]
    .join(" ")
    .trim();

  const {
    data: results,
    isLoading,
    isFetching,
  } = useQuery({
    ...getApiTorrentSearchOptions({
      query: { keyword: searchKeyword },
    }),
    enabled: debouncedKeyword.length > 0,
  });

  // Preview / file selection state
  const [previewState, setPreviewState] = useState<PreviewState>({
    status: "idle",
  });
  const [selectedIndices, setSelectedIndices] = useState<Set<number>>(
    () => new Set(),
  );
  const [collapsedPaths, setCollapsedPaths] = useState<Set<string>>(
    () => new Set(),
  );

  const pollAbortRef = useRef<AbortController | null>(null);

  const hasPreview = previewState.status !== "idle";

  // Cancel mutation (declared first so pollFiles can reference it)
  const cancelMutation = useMutation({
    ...deleteApiCollectionByHashMutation(),
  });

  const abortPolling = () => {
    pollAbortRef.current?.abort();
    pollAbortRef.current = null;
  };

  // Poll for files after torrent is added
  const pollFiles = async (hash: string) => {
    abortPolling();
    const controller = new AbortController();
    pollAbortRef.current = controller;

    for (let i = 0; i < MAX_POLLS; i++) {
      if (controller.signal.aborted) return;

      try {
        const { data } = await getApiCollectionFilesByHash({
          path: { hash },
          throwOnError: true,
        });

        if (controller.signal.aborted) return;

        if (data.files.length > 0) {
          setPreviewState({ status: "loaded", hash, data });
          const tree = buildFileTree(data.files);
          const excludedIndices = new Set([
            ...findQuickToggleFolders(tree).flatMap((f) => f.indices),
            ...(findQuickToggleContent(tree)?.indices ?? []),
          ]);
          setSelectedIndices(
            new Set(data.files.map((f) => f.index).filter((i) => !excludedIndices.has(i))),
          );
          const deepPaths = new Set<string>();
          const collectDeep = (nodes: FileTreeNode[], depth: number) => {
            for (const node of nodes) {
              if (!node.file && depth >= 1) deepPaths.add(node.path);
              if (!node.file) collectDeep(node.children, depth + 1);
            }
          };
          collectDeep(tree, 0);
          setCollapsedPaths(deepPaths);
          return;
        }
      } catch {
        if (controller.signal.aborted) return;
      }

      if (i < MAX_POLLS - 1) {
        await new Promise<void>((resolve) => {
          const timer = setTimeout(resolve, 1000);
          controller.signal.addEventListener(
            "abort",
            () => {
              clearTimeout(timer);
              resolve();
            },
            { once: true },
          );
        });
      }
    }

    // Timeout
    if (!controller.signal.aborted) {
      cancelMutation.mutate({ path: { hash } });
      setPreviewState({ status: "error" });
      toast.error("获取文件列表超时");
    }
  };

  // Mutations
  const addMutation = useMutation({
    ...postApiCollectionAddMutation(),
    onSuccess: (data) => {
      setPreviewState({ status: "polling", hash: data.hash });
      pollFiles(data.hash);
    },
    onError: () => {
      setPreviewState({ status: "error" });
      toast.error("添加种子失败");
    },
  });

  const confirmMutation = useMutation({
    ...postApiCollectionConfirmMutation(),
    onSuccess: () => {
      toast.success("下载已开始");
      resetPreview();
      setKeyword("");
      onOpenChange(false);
    },
    onError: () => {
      toast.error("确认下载失败");
    },
  });

  // Handlers (no useCallback - React Compiler handles memoization)
  const resetPreview = () => {
    abortPolling();
    setPreviewState({ status: "idle" });
    setSelectedIndices(new Set());
    setCollapsedPaths(new Set());
  };

  const cancelPreview = () => {
    if (
      previewState.status === "loaded" ||
      previewState.status === "polling"
    ) {
      cancelMutation.mutate({ path: { hash: previewState.hash } });
    }
    resetPreview();
  };

  const handleSelectResult = (torrentUrl: string, infoHash?: string) => {
    if (
      previewState.status === "loaded" ||
      previewState.status === "polling"
    ) {
      cancelMutation.mutate({ path: { hash: previewState.hash } });
    }
    abortPolling();
    setSelectedIndices(new Set());
    setCollapsedPaths(new Set());
    setPreviewState({ status: "adding" });
    addMutation.mutate({ body: { torrentUrl, infoHash } });
  };

  const handleConfirm = () => {
    if (previewState.status !== "loaded") return;
    const allIndices = previewState.data.files.map((f) => f.index);
    const unwantedIndices = allIndices.filter((i) => !selectedIndices.has(i));
    confirmMutation.mutate({
      body: { hash: previewState.hash, unwantedIndices },
    });
  };

  const toggleFile = (index: number) => {
    setSelectedIndices((prev) => {
      const next = new Set(prev);
      if (next.has(index)) next.delete(index);
      else next.add(index);
      return next;
    });
  };

  const toggleAll = () => {
    if (previewState.status !== "loaded") return;
    const allIndices = previewState.data.files.map((f) => f.index);
    setSelectedIndices((prev) =>
      prev.size === allIndices.length ? new Set() : new Set(allIndices),
    );
  };

  const toggleIndices = (indices: number[]) => {
    setSelectedIndices((prev) => {
      const allSelected = indices.every((i) => prev.has(i));
      const next = new Set(prev);
      if (allSelected) {
        for (const i of indices) next.delete(i);
      } else {
        for (const i of indices) next.add(i);
      }
      return next;
    });
  };

  const toggleCollapse = (path: string) => {
    setCollapsedPaths((prev) => {
      const next = new Set(prev);
      if (next.has(path)) next.delete(path);
      else next.add(path);
      return next;
    });
  };

  const handleOpenChange = (next: boolean) => {
    if (!next) {
      if (
        previewState.status === "loaded" ||
        previewState.status === "polling"
      ) {
        cancelMutation.mutate({ path: { hash: previewState.hash } });
      }
      resetPreview();
      setKeyword("");
    }
    onOpenChange(next);
  };

  return (
    <DialogPrimitive.Root open={open} onOpenChange={handleOpenChange}>
      <DialogPrimitive.Portal>
        <DialogPrimitive.Backdrop
          className={cn(
            "fixed inset-0 z-50",
            "bg-black/10 dark:bg-black/30 backdrop-blur-sm",
            "data-[state=open]:animate-in data-[state=closed]:animate-out",
            "data-[state=closed]:fade-out-0 data-[state=open]:fade-in-0",
            "duration-200",
          )}
        />
        <DialogPrimitive.Popup
          className={cn(
            "fixed left-1/2 top-[45%] z-50 -translate-y-1/2 -translate-x-1/2",
            "w-[calc(100%-2rem)]",
            "outline-hidden",
          )}
        >
          <motion.div
            className="mx-auto flex overflow-hidden rounded-2xl glass-search h-[70vh]"
            initial={false}
            animate={{ maxWidth: hasPreview ? "68rem" : "36rem" }}
            transition={{ type: "spring", stiffness: 400, damping: 35 }}
          >
            {/* Left panel: Search */}
            <div
              className={cn(
                "flex flex-col w-xl shrink-0",
                hasPreview && "border-r border-foreground/6",
              )}
            >
              <SearchPanel
                keyword={keyword}
                onKeywordChange={setKeyword}
                results={results}
                isLoading={isLoading}
                isFetching={isFetching}
                debouncedKeyword={debouncedKeyword}
                onSelectResult={handleSelectResult}
              />
            </div>

            {/* Right panel: File selection */}
            <AnimatePresence>
              {hasPreview && (
                <motion.div
                  initial={{ opacity: 0, width: 0 }}
                  animate={{ opacity: 1, width: "auto" }}
                  exit={{ opacity: 0, width: 0 }}
                  transition={{ type: "spring", stiffness: 400, damping: 35 }}
                  className="flex-1 flex flex-col min-w-0 overflow-hidden"
                  onKeyDown={(e) => e.stopPropagation()}
                >
                  <FilePanel
                    previewState={previewState}
                    selectedIndices={selectedIndices}
                    collapsedPaths={collapsedPaths}
                    onToggleFile={toggleFile}
                    onToggleAll={toggleAll}
                    onToggleIndices={toggleIndices}
                    onToggleCollapse={toggleCollapse}
                    onConfirm={handleConfirm}
                    onCancel={cancelPreview}
                    isConfirming={confirmMutation.isPending}
                    isCancelling={cancelMutation.isPending}
                  />
                </motion.div>
              )}
            </AnimatePresence>
          </motion.div>
        </DialogPrimitive.Popup>
      </DialogPrimitive.Portal>
    </DialogPrimitive.Root>
  );
}
