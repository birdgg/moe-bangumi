import {
  IconMovie,
  IconMusic,
  IconSubtask,
  IconPhoto,
  IconFile,
} from "@tabler/icons-react";
import type { TorrentFileDTO } from "@/client/types.gen";

export type FileCategory = "video" | "audio" | "subtitle" | "image" | "other";

export const CATEGORY_CONFIG: Record<
  FileCategory,
  { icon: typeof IconMovie; tint: string; text: string; label: string }
> = {
  video: {
    icon: IconMovie,
    tint: "bg-chart-1/12",
    text: "text-chart-1",
    label: "视频",
  },
  audio: {
    icon: IconMusic,
    tint: "bg-chart-3/12",
    text: "text-chart-3",
    label: "音频",
  },
  subtitle: {
    icon: IconSubtask,
    tint: "bg-chart-2/12",
    text: "text-chart-2",
    label: "字幕",
  },
  image: {
    icon: IconPhoto,
    tint: "bg-chart-4/12",
    text: "text-chart-4",
    label: "图片",
  },
  other: {
    icon: IconFile,
    tint: "bg-foreground/[0.04]",
    text: "text-muted-foreground",
    label: "其他",
  },
};

const VIDEO_EXTS = new Set([
  "mkv", "mp4", "avi", "ts", "m2ts", "flv", "wmv", "webm",
]);
const AUDIO_EXTS = new Set([
  "flac", "mka", "aac", "mp3", "wav", "ogg", "ac3", "dts",
]);
const SUBTITLE_EXTS = new Set(["ass", "srt", "sub", "ssa", "sup", "idx"]);
const IMAGE_EXTS = new Set(["png", "jpg", "jpeg", "bmp", "gif", "webp"]);

export function getFileCategory(name: string): FileCategory {
  const ext = name.split(".").pop()?.toLowerCase() ?? "";
  if (VIDEO_EXTS.has(ext)) return "video";
  if (AUDIO_EXTS.has(ext)) return "audio";
  if (SUBTITLE_EXTS.has(ext)) return "subtitle";
  if (IMAGE_EXTS.has(ext)) return "image";
  return "other";
}

export function formatFileSize(bytes: number): string {
  if (bytes < 1024) return `${bytes} B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)} KB`;
  if (bytes < 1024 * 1024 * 1024)
    return `${(bytes / (1024 * 1024)).toFixed(1)} MB`;
  return `${(bytes / (1024 * 1024 * 1024)).toFixed(2)} GB`;
}

export function getFileName(fullPath: string): string {
  const parts = fullPath.split("/");
  return parts[parts.length - 1] ?? fullPath;
}

export interface FileTreeNode {
  name: string;
  path: string;
  file?: TorrentFileDTO;
  children: FileTreeNode[];
}

export function buildFileTree(files: TorrentFileDTO[]): FileTreeNode[] {
  const root: FileTreeNode[] = [];
  for (const file of files) {
    const parts = file.name.split("/");
    let current = root;
    let pathSoFar = "";
    for (let i = 0; i < parts.length; i++) {
      const part = parts[i]!;
      pathSoFar = pathSoFar ? `${pathSoFar}/${part}` : part;
      if (i === parts.length - 1) {
        current.push({ name: part, path: pathSoFar, file, children: [] });
      } else {
        let dirNode = current.find((n) => n.name === part && !n.file);
        if (!dirNode) {
          dirNode = { name: part, path: pathSoFar, children: [] };
          current.push(dirNode);
        }
        current = dirNode.children;
      }
    }
  }
  return sortTreeNodes(root);
}

function sortTreeNodes(nodes: FileTreeNode[]): FileTreeNode[] {
  const folders = nodes
    .filter((n) => !n.file)
    .map((n) => ({ ...n, children: sortTreeNodes(n.children) }));
  const files = nodes.filter((n) => n.file);
  return [...folders, ...files];
}

export function collectFileIndices(node: FileTreeNode): number[] {
  if (node.file) return [node.file.index];
  return node.children.flatMap(collectFileIndices);
}

export function collectTreeSize(node: FileTreeNode): number {
  if (node.file) return node.file.size;
  return node.children.reduce((sum, c) => sum + collectTreeSize(c), 0);
}

export const listVariants = {
  visible: {
    transition: { staggerChildren: 0.03 },
  },
};

export const itemVariants = {
  hidden: { opacity: 0, y: 6 },
  visible: {
    opacity: 1,
    y: 0,
    transition: { duration: 0.2, ease: "easeOut" as const },
  },
};

export const collapseVariants = {
  open: { height: "auto", opacity: 1 },
  collapsed: { height: 0, opacity: 0 },
};
