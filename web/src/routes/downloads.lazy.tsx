import { createLazyFileRoute } from "@tanstack/react-router"
import { DownloadsPage } from "@/features/downloads/pages/downloads-page"

export const Route = createLazyFileRoute("/downloads")({
  component: DownloadsPage,
})
