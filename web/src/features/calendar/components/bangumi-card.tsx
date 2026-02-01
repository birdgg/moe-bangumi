import type { BangumiResponse } from "@/client/types.gen"

interface BangumiCardProps {
  bangumi: BangumiResponse
}

export function BangumiCard({ bangumi }: BangumiCardProps) {
  return (
    <div className="group w-44">
      <div className="liquid-glass-card relative overflow-hidden rounded-2xl">
        <div className="liquid-glass-refraction" />

        <div className="aspect-[2/3] overflow-hidden rounded-t-2xl">
          {bangumi.posterUrl ? (
            <img
              src={bangumi.posterUrl}
              alt={bangumi.titleChs}
              className="size-full object-cover transition-transform duration-300 ease-out group-hover:scale-110"
            />
          ) : (
            <div className="flex size-full items-center justify-center bg-gradient-to-br from-muted to-muted/50">
              <span className="text-muted-foreground text-sm">No Image</span>
            </div>
          )}
        </div>

        <div className="liquid-glass-content relative h-14 p-3">
          <h3 className="line-clamp-2 text-sm font-medium leading-snug text-foreground/90">
            {bangumi.titleChs}
          </h3>
        </div>
      </div>
    </div>
  )
}
