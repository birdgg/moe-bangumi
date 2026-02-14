import { IconAlertTriangle } from "@tabler/icons-react"
import { motion } from "framer-motion"

export function ErrorState({ error }: { error: Error }) {
  return (
    <div className="flex flex-col items-center justify-center py-20 text-center">
      <motion.div
        initial={{ opacity: 0, scale: 0.9 }}
        animate={{ opacity: 1, scale: 1 }}
        transition={{ duration: 0.4, ease: [0.33, 1, 0.68, 1] }}
      >
        <IconAlertTriangle className="size-8 text-destructive/40 mb-3" />
        <p className="text-sm text-destructive/80">{error.message}</p>
      </motion.div>
    </div>
  )
}
