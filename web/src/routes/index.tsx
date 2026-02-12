import { createFileRoute } from "@tanstack/react-router";
import { PosterWall } from "@/features/bangumi/components/poster-wall";

export const Route = createFileRoute("/")({
  component: HomePage,
});

function HomePage() {
  return (
    <div className="p-6">
      <h1 className="text-lg font-semibold text-foreground/80 mb-6">我的追番</h1>
      <PosterWall />
    </div>
  );
}
