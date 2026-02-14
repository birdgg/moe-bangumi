import { createLazyFileRoute } from "@tanstack/react-router"
import { HomePage } from "@/features/bangumi/pages/home-page"

export const Route = createLazyFileRoute("/")({
  component: HomePage,
})
