import { createLazyFileRoute } from "@tanstack/react-router"
import { LogsPage } from "@/features/logs/pages/logs-page"

export const Route = createLazyFileRoute("/logs")({
  component: LogsPage,
})
