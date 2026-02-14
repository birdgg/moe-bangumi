import { createFileRoute } from "@tanstack/react-router"
import { z } from "zod"

const searchSchema = z.object({
  section: z.enum([
    "general",
    "downloader",
    "filter",
    "proxy",
    "notification",
    "priority",
    "system",
  ]).optional().default("general"),
})

export const Route = createFileRoute("/settings")({
  validateSearch: searchSchema,
})
