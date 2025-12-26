import * as React from "react";
import { Dialog as DialogPrimitive } from "@base-ui/react/dialog";
import { AnimatePresence, motion } from "framer-motion";
import { cn } from "@/lib/utils";
import { Input } from "@/components/ui/input";
import {
  IconX,
  IconSearch,
  IconDownload,
} from "@tabler/icons-react";

interface TorrentSearchModalProps {
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onSelect: (magnetUrl: string) => void;
  initialKeyword?: string;
}

export function TorrentSearchModal({
  open,
  onOpenChange,
  onSelect: _onSelect,
  initialKeyword = "",
}: TorrentSearchModalProps) {
  // TODO: onSelect will be used when search functionality is implemented
  void _onSelect;
  const [keyword, setKeyword] = React.useState(initialKeyword);
  const inputRef = React.useRef<HTMLInputElement>(null);

  // Auto focus input when modal opens
  React.useEffect(() => {
    if (open) {
      setKeyword(initialKeyword);
      setTimeout(() => inputRef.current?.focus(), 100);
    }
  }, [open, initialKeyword]);

  return (
    <DialogPrimitive.Root open={open} onOpenChange={onOpenChange}>
      <AnimatePresence>
        {open && (
          <DialogPrimitive.Portal keepMounted>
            {/* Backdrop */}
            <DialogPrimitive.Backdrop
              render={
                <motion.div
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  exit={{ opacity: 0 }}
                  transition={{ duration: 0.2 }}
                  className={cn(
                    "fixed inset-0 z-[60]",
                    "bg-black/20 dark:bg-black/40 backdrop-blur-sm"
                  )}
                />
              }
            />

            {/* Modal */}
            <DialogPrimitive.Popup
              render={
                <motion.div
                  initial={{ opacity: 0, scale: 0.95, x: "calc(-50% + 32px)", y: "-50%" }}
                  animate={{ opacity: 1, scale: 1, x: "-50%", y: "-50%" }}
                  exit={{ opacity: 0, scale: 0.95, x: "calc(-50% + 32px)", y: "-50%" }}
                  transition={{
                    type: "spring",
                    damping: 25,
                    stiffness: 300,
                  }}
                  className={cn(
                    "fixed left-1/2 top-1/2 z-[60]",
                    "w-[calc(100%-2rem)] max-w-xl",
                    "max-h-[80vh] overflow-hidden",
                    "rounded-2xl",
                    "bg-linear-to-br from-white/95 via-white/90 to-chart-1/10",
                    "dark:from-zinc-900/95 dark:via-zinc-900/90 dark:to-chart-3/20",
                    "border border-chart-1/30 dark:border-chart-3/30",
                    "shadow-2xl shadow-chart-1/20 dark:shadow-chart-3/50",
                    "backdrop-blur-xl",
                    "outline-none"
                  )}
                />
              }
            >
              {/* Decorative glow */}
              <motion.div
                initial={{ opacity: 0, scale: 0.8 }}
                animate={{ opacity: 1, scale: 1 }}
                transition={{ delay: 0.1, duration: 0.3 }}
                className="pointer-events-none absolute -right-16 -top-16 size-32 rounded-full bg-linear-to-br from-chart-1/30 to-chart-3/30 blur-3xl dark:from-chart-1/20 dark:to-chart-3/20"
              />

              {/* Header */}
              <div className="relative border-b border-chart-1/30 dark:border-chart-3/20 px-4 py-3">
                <div className="flex items-center gap-2.5">
                  <motion.div
                    initial={{ scale: 0, rotate: -180 }}
                    animate={{ scale: 1, rotate: 0 }}
                    transition={{ type: "spring", damping: 15, stiffness: 300, delay: 0.1 }}
                    className="flex size-8 items-center justify-center rounded-lg bg-linear-to-br from-chart-1 to-chart-3 text-white shadow-md shadow-chart-1/30"
                  >
                    <IconSearch className="size-4" />
                  </motion.div>
                  <DialogPrimitive.Title className="flex-1 text-base font-semibold bg-linear-to-r from-chart-1 via-chart-3 to-chart-5 bg-clip-text text-transparent">
                    搜索种子
                  </DialogPrimitive.Title>
                  <DialogPrimitive.Close
                    className={cn(
                      "flex size-7 items-center justify-center rounded-md",
                      "text-muted-foreground hover:text-foreground",
                      "hover:bg-chart-1/20 dark:hover:bg-chart-3/30",
                      "transition-colors duration-200",
                      "outline-none focus-visible:ring-2 focus-visible:ring-chart-1 dark:focus-visible:ring-chart-3"
                    )}
                  >
                    <IconX className="size-4" />
                  </DialogPrimitive.Close>
                </div>
              </div>

              {/* Content */}
              <motion.div
                initial={{ opacity: 0, y: 10 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ delay: 0.15, duration: 0.2 }}
                className="relative p-4 space-y-4"
              >
                {/* Search Input */}
                <div className="relative">
                  <IconSearch className="absolute left-3 top-1/2 -translate-y-1/2 size-4 text-muted-foreground" />
                  <Input
                    ref={inputRef}
                    value={keyword}
                    onChange={(e) => setKeyword(e.target.value)}
                    placeholder="输入番剧名称搜索..."
                    className="pl-9"
                  />
                </div>

                {/* Placeholder content */}
                <motion.div
                  initial={{ opacity: 0, scale: 0.9 }}
                  animate={{ opacity: 1, scale: 1 }}
                  transition={{ delay: 0.2, type: "spring", damping: 20 }}
                  className="flex flex-col items-center justify-center py-12 text-muted-foreground"
                >
                  <div className="flex size-16 items-center justify-center rounded-full bg-chart-1/10 dark:bg-chart-3/10 mb-4">
                    <IconDownload className="size-8 text-chart-1 dark:text-chart-3" />
                  </div>
                  <p className="text-sm font-medium">种子搜索功能即将推出</p>
                  <p className="text-xs text-muted-foreground/70 mt-1">
                    支持 Mikan、Nyaa 等站点搜索
                  </p>
                </motion.div>
              </motion.div>
            </DialogPrimitive.Popup>
          </DialogPrimitive.Portal>
        )}
      </AnimatePresence>
    </DialogPrimitive.Root>
  );
}
