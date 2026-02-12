import { createFileRoute } from "@tanstack/react-router"
import { CalendarView } from "@/features/calendar"

export const Route = createFileRoute("/schedule")({
  component: SchedulePage,
})

function SchedulePage() {
  return (
    <div className="p-6">
      <CalendarView />
    </div>
  )
}
