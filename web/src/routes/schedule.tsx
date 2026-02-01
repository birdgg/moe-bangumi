import { createFileRoute } from "@tanstack/react-router"
import { CalendarView } from "@/features/calendar"

export const Route = createFileRoute("/schedule")({
  component: SchedulePage,
})

function SchedulePage() {
  return (
    <div className="p-6">
      <h1 className="mb-6 text-2xl font-bold">每日放送</h1>
      <CalendarView />
    </div>
  )
}
