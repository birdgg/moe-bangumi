import { Skeleton } from "@/components/ui/skeleton"
import { motion } from "framer-motion"

export function LoadingSkeleton() {
  return (
    <div className="p-4 space-y-2">
      {Array.from({ length: 12 }).map((_, i) => (
        <motion.div
          key={i}
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: i * 0.04 }}
          className="grid grid-cols-[3.5rem_auto_1fr_4.5rem] items-center gap-x-3"
        >
          <Skeleton className="h-3.5 w-12 rounded" />
          <div className="flex items-center gap-1.5">
            <Skeleton className="size-2.5 rounded-full" />
            <Skeleton className="h-3 w-10 rounded" />
          </div>
          <Skeleton className="h-3.5 rounded" style={{ width: `${40 + Math.random() * 50}%` }} />
          <Skeleton className="h-3 w-8 rounded ml-auto" />
        </motion.div>
      ))}
    </div>
  )
}
