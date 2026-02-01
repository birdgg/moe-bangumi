import type { BangumiResponse } from "@/client/types.gen"

interface BangumiCardProps {
  bangumi: BangumiResponse
}

export function BangumiCard({ bangumi }: BangumiCardProps) {
  return (
    <div className="group relative w-32 shrink-0 overflow-hidden rounded-lg bg-card ring-1 ring-foreground/10 transition-all hover:ring-chart-1/50 hover:shadow-lg">
      <div className="aspect-[2/3] overflow-hidden">
        {bangumi.posterUrl ? (
          <img
            src={bangumi.posterUrl}
            alt={bangumi.titleChs}
            className="size-full object-cover transition-transform duration-300 group-hover:scale-105"
          />
        ) : (
          <div className="flex size-full items-center justify-center bg-muted">
            <span className="text-muted-foreground text-xs">No Image</span>
          </div>
        )}
      </div>
      <div className="p-2">
        <h3 className="line-clamp-2 text-xs font-medium leading-tight">
          {bangumi.titleChs}
        </h3>
      </div>
    </div>
  )
}
