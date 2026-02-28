import { useState } from "react"
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from "@/components/ui/alert-dialog"
import { Checkbox } from "@/components/ui/checkbox"

interface DeleteDialogProps {
  open: boolean
  onOpenChange: (open: boolean) => void
  onConfirm: (deleteFiles: boolean) => void
  count: number
}

export function DeleteDialog({
  open,
  onOpenChange,
  onConfirm,
  count,
}: DeleteDialogProps) {
  const [deleteFiles, setDeleteFiles] = useState(false)

  const handleConfirm = () => {
    onConfirm(deleteFiles)
    setDeleteFiles(false)
  }

  return (
    <AlertDialog open={open} onOpenChange={onOpenChange}>
      <AlertDialogContent size="sm">
        <AlertDialogHeader>
          <AlertDialogTitle>
            确认删除 {count} 个任务？
          </AlertDialogTitle>
          <AlertDialogDescription>
            此操作无法撤销。
          </AlertDialogDescription>
        </AlertDialogHeader>

        <label className="flex items-center gap-2 px-1 cursor-pointer">
          <Checkbox
            checked={deleteFiles}
            onCheckedChange={(checked) => setDeleteFiles(checked === true)}
          />
          <span className="text-sm text-muted-foreground">同时删除文件</span>
        </label>

        <AlertDialogFooter>
          <AlertDialogCancel>取消</AlertDialogCancel>
          <AlertDialogAction variant="destructive" onClick={handleConfirm}>
            删除
          </AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  )
}
