import { useMemo } from "react";
import { motion, AnimatePresence } from "framer-motion";
import { Button } from "@/components/ui/button";
import { Checkbox } from "@/components/ui/checkbox";
import { Spinner } from "@/components/ui/spinner";
import { IconChevronRight, IconFolder } from "@tabler/icons-react";
import type { CollectionFilesResponse, TorrentFileDTO } from "@/client/types.gen";
import {
  type FileTreeNode,
  CATEGORY_CONFIG,
  getFileCategory,
  formatFileSize,
  getFileName,
  buildFileTree,
  collectFileIndices,
  collectTreeSize,
  listVariants,
  itemVariants,
  collapseVariants,
} from "./file-tree-utils";

function FileRow({
  file,
  checked,
  onToggle,
  depth = 0,
}: {
  file: TorrentFileDTO;
  checked: boolean;
  onToggle: () => void;
  depth?: number;
}) {
  const category = getFileCategory(file.name);
  const config = CATEGORY_CONFIG[category];
  const Icon = config.icon;

  return (
    <motion.label
      variants={itemVariants}
      className="group flex items-center rounded-xl pr-3 py-2.5 cursor-pointer select-none transition-colors duration-150 hover:bg-foreground/[0.03]"
      style={{ paddingLeft: `${12 + depth * 20}px` }}
    >
      <div className="flex items-center gap-3 min-w-0 flex-1">
        {depth > 0 && <div className="shrink-0 w-4" />}

        <Checkbox
          checked={checked}
          onCheckedChange={onToggle}
          className="shrink-0"
        />

        <div
          className={`shrink-0 flex items-center justify-center size-6 rounded-lg ${config.tint}`}
        >
          <Icon className={`size-3.5 ${config.text}`} />
        </div>

        <div className="min-w-0 flex-1">
          <p className="text-[12.5px] text-foreground/70 break-all leading-relaxed group-hover:text-foreground/90 transition-colors">
            {getFileName(file.name)}
          </p>
        </div>

        <span className="shrink-0 text-[10px] text-foreground/25 tabular-nums font-medium">
          {formatFileSize(file.size)}
        </span>
      </div>
    </motion.label>
  );
}

function FolderRow({
  node,
  depth,
  collapsed,
  onToggleCollapse,
  selectedIndices,
  onToggleFolder,
}: {
  node: FileTreeNode;
  depth: number;
  collapsed: boolean;
  onToggleCollapse: () => void;
  selectedIndices: Set<number>;
  onToggleFolder: (indices: number[]) => void;
}) {
  const indices = useMemo(() => collectFileIndices(node), [node]);
  const totalSize = useMemo(() => collectTreeSize(node), [node]);
  const selectedCount = indices.filter((i) => selectedIndices.has(i)).length;
  const allSelected = indices.length > 0 && selectedCount === indices.length;
  const someSelected = selectedCount > 0 && !allSelected;

  return (
    <motion.div
      variants={itemVariants}
      className="group flex items-center rounded-xl pr-3 py-2 select-none transition-colors duration-150 hover:bg-foreground/[0.03]"
      style={{ paddingLeft: `${12 + depth * 20}px` }}
    >
      <div className="relative flex items-center gap-3 min-w-0 flex-1">
        <div
          className="shrink-0 flex items-center justify-center w-4 cursor-pointer"
          onClick={onToggleCollapse}
        >
          <IconChevronRight
            className={`size-3 text-foreground/30 transition-transform duration-200 ${!collapsed ? "rotate-90" : ""}`}
          />
        </div>

        <Checkbox
          checked={allSelected}
          indeterminate={someSelected}
          onCheckedChange={() => onToggleFolder(indices)}
          className="shrink-0"
        />

        <div className="shrink-0 flex items-center justify-center size-6 rounded-lg bg-foreground/[0.04]">
          <IconFolder className="size-3.5 text-foreground/40" />
        </div>

        <div className="min-w-0 flex-1">
          <p className="text-[12.5px] text-foreground/60 font-medium truncate">
            {node.name}
          </p>
        </div>

        <span className="shrink-0 text-[10px] text-foreground/20 tabular-nums font-medium">
          {indices.length} · {formatFileSize(totalSize)}
        </span>
      </div>
    </motion.div>
  );
}

function TreeNodes({
  nodes,
  depth,
  collapsedPaths,
  toggleCollapse,
  selectedIndices,
  toggleFile,
  toggleIndices,
}: {
  nodes: FileTreeNode[];
  depth: number;
  collapsedPaths: Set<string>;
  toggleCollapse: (path: string) => void;
  selectedIndices: Set<number>;
  toggleFile: (index: number) => void;
  toggleIndices: (indices: number[]) => void;
}) {
  return (
    <>
      {nodes.map((node) =>
        node.file ? (
          <FileRow
            key={node.path}
            file={node.file}
            checked={selectedIndices.has(node.file.index)}
            onToggle={() => toggleFile(node.file!.index)}
            depth={depth}
          />
        ) : (
          <div key={node.path}>
            <FolderRow
              node={node}
              depth={depth}
              collapsed={collapsedPaths.has(node.path)}
              onToggleCollapse={() => toggleCollapse(node.path)}
              selectedIndices={selectedIndices}
              onToggleFolder={toggleIndices}
            />
            <AnimatePresence initial={false}>
              {!collapsedPaths.has(node.path) && (
                <motion.div
                  key="children"
                  initial="collapsed"
                  animate="open"
                  exit="collapsed"
                  variants={collapseVariants}
                  transition={{ duration: 0.2, ease: "easeInOut" }}
                  className="overflow-hidden"
                >
                  <TreeNodes
                    nodes={node.children}
                    depth={depth + 1}
                    collapsedPaths={collapsedPaths}
                    toggleCollapse={toggleCollapse}
                    selectedIndices={selectedIndices}
                    toggleFile={toggleFile}
                    toggleIndices={toggleIndices}
                  />
                </motion.div>
              )}
            </AnimatePresence>
          </div>
        ),
      )}
    </>
  );
}

export type PreviewState =
  | { status: "idle" }
  | { status: "adding" }
  | { status: "polling"; hash: string }
  | { status: "loaded"; hash: string; data: CollectionFilesResponse }
  | { status: "error" };

interface FilePanelProps {
  previewState: PreviewState;
  selectedIndices: Set<number>;
  collapsedPaths: Set<string>;
  onToggleFile: (index: number) => void;
  onToggleAll: () => void;
  onToggleIndices: (indices: number[]) => void;
  onToggleCollapse: (path: string) => void;
  onConfirm: () => void;
  onCancel: () => void;
  isConfirming: boolean;
  isCancelling: boolean;
}

export function FilePanel({
  previewState,
  selectedIndices,
  collapsedPaths,
  onToggleFile,
  onToggleAll,
  onToggleIndices,
  onToggleCollapse,
  onConfirm,
  onCancel,
  isConfirming,
  isCancelling,
}: FilePanelProps) {
  const preview =
    previewState.status === "loaded" ? previewState.data : null;

  const fileTree = useMemo(
    () => (preview ? buildFileTree(preview.files) : []),
    [preview],
  );

  const isLoading = isConfirming || isCancelling;
  const allSelected = preview
    ? selectedIndices.size === preview.files.length
    : false;
  const someSelected = selectedIndices.size > 0 && !allSelected;

  if (previewState.status === "adding") {
    return (
      <div className="flex flex-col items-center justify-center flex-1 py-14">
        <Spinner className="size-5 text-foreground/25" />
        <p className="mt-3 text-xs text-foreground/25">正在添加种子…</p>
      </div>
    );
  }

  if (previewState.status === "polling") {
    return (
      <div className="flex flex-col items-center justify-center flex-1 py-14">
        <Spinner className="size-5 text-foreground/25" />
        <p className="mt-3 text-xs text-foreground/25">正在获取文件列表…</p>
      </div>
    );
  }

  if (previewState.status === "error") {
    return (
      <div className="flex flex-col items-center justify-center flex-1 py-14">
        <p className="text-xs text-foreground/35">获取文件列表失败</p>
        <Button variant="ghost" size="sm" onClick={onCancel} className="mt-2 text-foreground/50">
          返回
        </Button>
      </div>
    );
  }

  if (!preview) return null;

  return (
    <div className="flex flex-col h-full">
      {/* Header */}
      <div className="relative px-4 pt-4 pb-3">
        <div className="absolute inset-x-0 top-0 h-16 rounded-tr-2xl pointer-events-none bg-[radial-gradient(ellipse_80%_50%_at_50%_-20%,oklch(1_0_0_/_0.12),transparent_70%)] dark:bg-[radial-gradient(ellipse_80%_50%_at_50%_-20%,oklch(1_0_0_/_0.04),transparent_70%)]" />
        <div className="relative">
          <h3 className="text-sm font-medium text-foreground/90">
            选择下载文件
          </h3>
        </div>
      </div>

      {/* Select all bar */}
      <div className="flex items-center gap-2.5 mx-3 px-3 py-2 rounded-lg">
        <Checkbox
          checked={allSelected}
          indeterminate={someSelected}
          onCheckedChange={onToggleAll}
        />
        <span className="text-[11px] text-foreground/50 font-medium">
          全选
        </span>
        <span className="ml-auto text-[10px] text-foreground/25 tabular-nums">
          {selectedIndices.size}/{preview.files.length}
        </span>
      </div>

      {/* Divider */}
      <div className="relative h-px mx-4 mt-2">
        <div className="absolute inset-0 bg-foreground/[0.06]" />
        <div className="absolute inset-0 bg-linear-to-r from-transparent via-foreground/10 to-transparent" />
      </div>

      {/* File list */}
      <div className="flex-1 overflow-y-auto scrollbar-none px-1.5 py-1.5 scroll-smooth">
        <AnimatePresence mode="wait">
          <motion.div
            key={previewState.status === "loaded" ? previewState.hash : undefined}
            initial="hidden"
            animate="visible"
            variants={listVariants}
          >
            <TreeNodes
              nodes={fileTree}
              depth={0}
              collapsedPaths={collapsedPaths}
              toggleCollapse={onToggleCollapse}
              selectedIndices={selectedIndices}
              toggleFile={onToggleFile}
              toggleIndices={onToggleIndices}
            />
          </motion.div>
        </AnimatePresence>
      </div>

      {/* Actions */}
      <div className="border-t border-foreground/[0.06]">
        <div className="flex items-center justify-end gap-2 px-4 py-3">
          <Button
            variant="ghost"
            size="sm"
            onClick={onCancel}
            disabled={isLoading}
            className="text-foreground/50"
          >
            取消
          </Button>
          <Button
            size="sm"
            onClick={onConfirm}
            disabled={isLoading || selectedIndices.size === 0}
          >
            {isLoading ? (
              <Spinner className="size-3.5" />
            ) : (
              "确认下载"
            )}
          </Button>
        </div>
      </div>
    </div>
  );
}
