import { createFileRoute } from "@tanstack/react-router";
import { SchedulePage } from "@/features/schedule/page";

export const Route = createFileRoute("/schedule")({
  component: SchedulePage,
});
