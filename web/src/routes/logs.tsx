import { createFileRoute } from "@tanstack/react-router"
import { z } from "zod"
import { format } from "date-fns"

const searchSchema = z.object({
  date: z
    .string()
    .optional()
    .default(format(new Date(), "yyyy-MM-dd")),
  page: z.number().optional().default(1),
  pageSize: z.number().optional().default(50),
})

export const Route = createFileRoute("/logs")({
  validateSearch: searchSchema,
})
