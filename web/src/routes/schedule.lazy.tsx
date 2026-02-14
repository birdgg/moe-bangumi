import { createLazyFileRoute } from "@tanstack/react-router"
import { SchedulePage } from "@/features/calendar/pages/schedule-page"

export const Route = createLazyFileRoute("/schedule")({
  component: SchedulePage,
})
