import { IconTerminal2 } from "@tabler/icons-react"
import { motion } from "framer-motion"

export function EmptyState({ date }: { date: string }) {
  return (
    <div className="flex flex-col items-center justify-center py-20 text-center">
      <motion.div
        initial={{ opacity: 0, scale: 0.9 }}
        animate={{ opacity: 1, scale: 1 }}
        transition={{ duration: 0.4, ease: [0.33, 1, 0.68, 1] }}
      >
        <IconTerminal2 className="size-8 text-muted-foreground/20 mb-3" />
        <p className="text-sm text-muted-foreground/60">
          {date} - no log entries
        </p>
      </motion.div>
    </div>
  )
}
