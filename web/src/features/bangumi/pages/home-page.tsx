import { PosterWall } from "../components/poster-wall"

export function HomePage() {
  return (
    <div className="p-6">
      <h1 className="text-lg font-semibold text-foreground/80 mb-6">我的追番</h1>
      <PosterWall />
    </div>
  )
}
