import { createLazyFileRoute } from "@tanstack/react-router"
import { SettingsPage } from "@/features/settings/pages/settings-page"

export const Route = createLazyFileRoute("/settings")({
  component: SettingsPage,
})
