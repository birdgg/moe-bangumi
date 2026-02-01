import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/schedule")({
  component: SchedulePage,
});

function SchedulePage() {
  return (
    <div className="p-6">
      <h1 className="text-2xl font-bold">Schedule</h1>
      <p className="mt-2 text-muted-foreground">Coming soon...</p>
    </div>
  );
}
