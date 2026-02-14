import { format, subDays, addDays, isToday } from "date-fns"
import { Calendar } from "@/components/ui/calendar"
import { Button } from "@/components/ui/button"
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from "@/components/ui/popover"
import {
  IconChevronLeft,
  IconChevronRight,
  IconCalendar,
  IconAlertTriangle,
} from "@tabler/icons-react"
import { cn } from "@/lib/utils"
import { motion } from "framer-motion"
import { formatDateLabel } from "../utils/format"

interface DateNavigationProps {
  date: string
  onDateChange: (date: string) => void
  warnCount: number
}

export function DateNavigation({ date, onDateChange, warnCount }: DateNavigationProps) {
  const currentDate = new Date(date + "T00:00:00")
  const canGoForward = !isToday(currentDate)

  const goToPrevDay = () =>
    onDateChange(format(subDays(currentDate, 1), "yyyy-MM-dd"))
  const goToNextDay = () => {
    if (canGoForward) {
      onDateChange(format(addDays(currentDate, 1), "yyyy-MM-dd"))
    }
  }

  return (
    <motion.div
      initial={{ opacity: 0, y: -4 }}
      animate={{ opacity: 1, y: 0 }}
      transition={{
        duration: 0.4,
        delay: 0.05,
        ease: [0.33, 1, 0.68, 1],
      }}
      className="flex items-center justify-between mb-4"
    >
      <div className="flex items-center gap-1.5">
        <Button
          variant="ghost"
          size="icon-sm"
          onClick={goToPrevDay}
        >
          <IconChevronLeft className="size-4" />
        </Button>

        <Popover>
          <PopoverTrigger
            className={cn(
              "inline-flex items-center gap-2 px-3 h-7 rounded-lg",
              "text-sm font-medium tabular-nums",
              "hover:bg-muted/60 transition-colors cursor-pointer"
            )}
          >
            <IconCalendar className="size-3.5 text-muted-foreground" />
            <span>{date}</span>
            <span className="text-muted-foreground text-xs">
              {formatDateLabel(date)}
            </span>
          </PopoverTrigger>
          <PopoverContent align="start" sideOffset={8} className="w-auto p-0">
            <Calendar
              mode="single"
              selected={currentDate}
              onSelect={(d) => {
                if (d) onDateChange(format(d, "yyyy-MM-dd"))
              }}
              disabled={{ after: new Date() }}
            />
          </PopoverContent>
        </Popover>

        <Button
          variant="ghost"
          size="icon-sm"
          onClick={goToNextDay}
          disabled={!canGoForward}
        >
          <IconChevronRight className="size-4" />
        </Button>
      </div>

      {warnCount > 0 && (
        <motion.div
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.2 }}
          className="flex items-center gap-3 text-xs text-muted-foreground"
        >
          <span className="inline-flex items-center gap-1 text-destructive">
            <IconAlertTriangle className="size-3" />
            {warnCount}
          </span>
        </motion.div>
      )}
    </motion.div>
  )
}
