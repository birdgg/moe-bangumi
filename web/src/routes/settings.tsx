import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/settings")({
  component: SettingsPage,
});

function SettingsPage() {
  return (
    <div className="p-6">
      <h1 className="text-2xl font-bold">Settings</h1>
      <p className="mt-2 text-muted-foreground">Coming soon...</p>
    </div>
  );
}
