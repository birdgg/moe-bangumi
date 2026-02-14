import { Button } from "@/components/ui/button"
import { IconChevronLeft, IconChevronRight } from "@tabler/icons-react"
import { motion, AnimatePresence } from "framer-motion"

interface PaginationProps {
  page: number
  totalPages: number
  pageSize: number
  total: number
  onPageChange: (page: number) => void
}

export function Pagination({ page, totalPages, pageSize, total, onPageChange }: PaginationProps) {
  return (
    <AnimatePresence>
      {total > pageSize && (
        <motion.div
          initial={{ opacity: 0, y: 4 }}
          animate={{ opacity: 1, y: 0 }}
          exit={{ opacity: 0, y: 4 }}
          transition={{ duration: 0.3, delay: 0.15 }}
          className="flex items-center justify-between mt-4 px-1"
        >
          <p className="text-xs text-muted-foreground tabular-nums">
            {(page - 1) * pageSize + 1}-
            {Math.min(page * pageSize, total)} / {total}
          </p>
          <div className="flex items-center gap-1">
            <Button
              variant="ghost"
              size="icon-sm"
              disabled={page <= 1}
              onClick={() => onPageChange(page - 1)}
            >
              <IconChevronLeft className="size-3.5" />
            </Button>
            <span className="text-xs text-muted-foreground min-w-[4rem] text-center tabular-nums">
              {page} / {totalPages}
            </span>
            <Button
              variant="ghost"
              size="icon-sm"
              disabled={page >= totalPages}
              onClick={() => onPageChange(page + 1)}
            >
              <IconChevronRight className="size-3.5" />
            </Button>
          </div>
        </motion.div>
      )}
    </AnimatePresence>
  )
}
