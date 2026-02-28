import { IconDownload } from "@tabler/icons-react"
import {
  useDownloads,
  usePauseDownloads,
  useResumeDownloads,
  useDeleteDownloads,
} from "../hooks/use-downloads"
import { DownloadTable } from "../components/download-table"
import { Skeleton } from "@/components/ui/skeleton"

function LoadingSkeleton() {
  return (
    <div className="p-4 space-y-3">
      {Array.from({ length: 5 }).map((_, i) => (
        <Skeleton key={i} className="h-10 w-full rounded-lg" />
      ))}
    </div>
  )
}

function EmptyState() {
  return (
    <div className="flex flex-col items-center justify-center py-16 text-muted-foreground">
      <IconDownload className="size-10 mb-3 opacity-30" />
      <p className="text-sm">暂无下载任务</p>
    </div>
  )
}

function ErrorState({ error }: { error: Error }) {
  return (
    <div className="flex flex-col items-center justify-center py-16 text-destructive">
      <p className="text-sm">加载失败: {error.message}</p>
    </div>
  )
}

export function DownloadsPage() {
  const { data, isLoading, error } = useDownloads()
  const pauseMutation = usePauseDownloads()
  const resumeMutation = useResumeDownloads()
  const deleteMutation = useDeleteDownloads()

  const handlePause = (hashes: string[]) => {
    pauseMutation.mutate({ hashes })
  }

  const handleResume = (hashes: string[]) => {
    resumeMutation.mutate({ hashes })
  }

  const handleDelete = (hashes: string[], deleteFiles: boolean) => {
    deleteMutation.mutate({ hashes, deleteFiles })
  }

  return (
    <div className="min-h-full">
      <div className="px-6 py-8 md:px-8">
        <div className="max-w-5xl mx-auto">
          <div className="mb-6">
            <h1 className="text-lg font-semibold">下载</h1>
            {data && (
              <p className="text-sm text-muted-foreground mt-1">
                {data.length} 个任务
              </p>
            )}
          </div>

          <div className="liquid-glass-card rounded-2xl overflow-hidden relative">
            <div className="liquid-glass-refraction" />
            <div className="relative">
              {isLoading ? (
                <LoadingSkeleton />
              ) : error ? (
                <ErrorState error={error} />
              ) : !data || data.length === 0 ? (
                <EmptyState />
              ) : (
                <DownloadTable
                  items={data}
                  onPause={handlePause}
                  onResume={handleResume}
                  onDelete={handleDelete}
                />
              )}
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
