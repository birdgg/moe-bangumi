import { useState } from "react"
import { Badge } from "@/components/ui/badge"
import { Button } from "@/components/ui/button"
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"
import {
  IconDots,
  IconPlayerPause,
  IconPlayerPlay,
  IconTrash,
} from "@tabler/icons-react"
import type { DownloadItem } from "../types"
import {
  formatBytes,
  formatEta,
  formatSpeed,
  getStatusInfo,
  isPaused,
} from "../utils/format"
import { DeleteDialog } from "./delete-dialog"

interface DownloadTableProps {
  items: DownloadItem[]
  onPause: (hashes: string[]) => void
  onResume: (hashes: string[]) => void
  onDelete: (hashes: string[], deleteFiles: boolean) => void
}

function ProgressBar({ value }: { value: number }) {
  const percent = Math.round(value * 100)
  return (
    <div className="flex items-center gap-2">
      <div className="h-1.5 w-20 rounded-full bg-foreground/10 overflow-hidden">
        <div
          className="h-full rounded-full bg-chart-1 transition-all duration-300"
          style={{ width: `${percent}%` }}
        />
      </div>
      <span className="text-xs text-muted-foreground tabular-nums w-9 text-right">
        {percent}%
      </span>
    </div>
  )
}

function DownloadRow({
  item,
  onPause,
  onResume,
  onDeleteRequest,
}: {
  item: DownloadItem
  onPause: (hashes: string[]) => void
  onResume: (hashes: string[]) => void
  onDeleteRequest: (hash: string) => void
}) {
  const status = getStatusInfo(item.state)
  const paused = isPaused(item.state)

  return (
    <div className="grid grid-cols-[1fr_80px_140px_80px_72px_80px_36px] items-center gap-3 px-4 py-3 border-b border-foreground/[0.04] last:border-b-0 hover:bg-foreground/[0.02] transition-colors">
      <div className="min-w-0">
        <p className="text-sm truncate" title={item.name}>
          {item.name}
        </p>
      </div>

      <span className="text-xs text-muted-foreground tabular-nums">
        {formatBytes(item.size)}
      </span>

      <ProgressBar value={item.progress} />

      <div>
        <Badge variant={status.variant}>{status.label}</Badge>
      </div>

      <span className="text-xs text-muted-foreground tabular-nums">
        {item.ratio.toFixed(2)}
      </span>

      <div>
        <span className="text-xs text-muted-foreground tabular-nums">
          {item.dlspeed > 0
            ? formatSpeed(item.dlspeed)
            : item.upspeed > 0
              ? `↑ ${formatSpeed(item.upspeed)}`
              : formatEta(item.eta)}
        </span>
      </div>

      <DropdownMenu>
        <DropdownMenuTrigger
          render={
            <Button variant="ghost" size="icon-xs">
              <IconDots className="size-4" />
            </Button>
          }
        />
        <DropdownMenuContent align="end">
          {paused ? (
            <DropdownMenuItem onClick={() => onResume([item.hash])}>
              <IconPlayerPlay className="size-4" />
              继续
            </DropdownMenuItem>
          ) : (
            <DropdownMenuItem onClick={() => onPause([item.hash])}>
              <IconPlayerPause className="size-4" />
              暂停
            </DropdownMenuItem>
          )}
          <DropdownMenuSeparator />
          <DropdownMenuItem
            variant="destructive"
            onClick={() => onDeleteRequest(item.hash)}
          >
            <IconTrash className="size-4" />
            删除
          </DropdownMenuItem>
        </DropdownMenuContent>
      </DropdownMenu>
    </div>
  )
}

export function DownloadTable({
  items,
  onPause,
  onResume,
  onDelete,
}: DownloadTableProps) {
  const [deleteHash, setDeleteHash] = useState<string | null>(null)

  const handleDeleteConfirm = (deleteFiles: boolean) => {
    if (deleteHash) {
      onDelete([deleteHash], deleteFiles)
      setDeleteHash(null)
    }
  }

  return (
    <>
      <div className="px-4 py-2.5 border-b border-foreground/[0.06]">
        <div className="grid grid-cols-[1fr_80px_140px_80px_72px_80px_36px] items-center gap-3 text-xs font-medium text-muted-foreground">
          <span>名称</span>
          <span>大小</span>
          <span>进度</span>
          <span>状态</span>
          <span>比率</span>
          <span>下载速度</span>
          <span />
        </div>
      </div>

      {items.map((item) => (
        <DownloadRow
          key={item.hash}
          item={item}
          onPause={onPause}
          onResume={onResume}
          onDeleteRequest={setDeleteHash}
        />
      ))}

      <DeleteDialog
        open={deleteHash !== null}
        onOpenChange={(open) => {
          if (!open) setDeleteHash(null)
        }}
        onConfirm={handleDeleteConfirm}
        count={1}
      />
    </>
  )
}
