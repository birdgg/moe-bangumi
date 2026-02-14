import { useNavigate, useSearch } from "@tanstack/react-router"
import { toast } from "sonner"
import { Button } from "@/components/ui/button"
import { IconDeviceFloppy, IconLoader2 } from "@tabler/icons-react"
import {
  MetadataSection,
  DownloaderSection,
  FilterSection,
  ProxySection,
  NotificationSection,
  PrioritySection,
  SystemSection,
  SettingsSidebar,
  type SettingsSection,
} from "../components"
import { useSettingsForm } from "../hooks"

export function SettingsPage() {
  const { section: activeSection } = useSearch({ from: "/settings" })
  const navigate = useNavigate({ from: "/settings" })
  const { form, mutation, isLoading, error } = useSettingsForm()

  const setActiveSection = (section: SettingsSection) => {
    navigate({ search: { section } })
  }

  const handleSave = () => {
    const values = form.state.values
    mutation.mutate(values, {
      onSuccess: () => {
        toast.success("设置已保存")
      },
      onError: (error) => {
        toast.error("保存失败", {
          description: error instanceof Error ? error.message : "未知错误",
        })
      },
    })
  }

  const renderContent = () => {
    if (isLoading) {
      return (
        <div className="flex items-center justify-center py-12">
          <IconLoader2 className="size-5 animate-spin text-muted-foreground" />
        </div>
      )
    }

    if (error) {
      return (
        <div className="flex flex-col items-center justify-center py-12 text-center">
          <p className="text-sm text-destructive">
            加载设置失败: {error instanceof Error ? error.message : "未知错误"}
          </p>
        </div>
      )
    }

    switch (activeSection) {
      case "general":
        return <MetadataSection form={form} />
      case "downloader":
        return <DownloaderSection form={form} />
      case "priority":
        return <PrioritySection form={form} />
      case "filter":
        return <FilterSection form={form} />
      case "proxy":
        return <ProxySection form={form} />
      case "notification":
        return <NotificationSection form={form} />
      case "system":
        return <SystemSection />
      default:
        return <MetadataSection form={form} />
    }
  }

  return (
    <div className="min-h-full">
      <div className="px-6 py-8 md:px-8">
        <div className="max-w-7xl mx-auto">
          <div className="mb-8">
            <h1 className="text-2xl font-bold tracking-tight">设置</h1>
            <p className="text-sm text-muted-foreground mt-1">
              管理应用程序配置和偏好设置
            </p>
          </div>

          <div className="flex gap-8">
            <SettingsSidebar
              activeSection={activeSection}
              onSectionChange={setActiveSection}
            />

            <div className="flex-1 min-w-0">
              <form
                className="space-y-6"
                onSubmit={(e) => {
                  e.preventDefault()
                  e.stopPropagation()
                  handleSave()
                }}
              >
                <div className="rounded-2xl border bg-card p-6">
                  {renderContent()}
                </div>

                <div className="flex justify-end">
                  <Button
                    type="submit"
                    disabled={mutation.isPending || isLoading}
                    className="cursor-pointer relative gap-2 overflow-hidden bg-linear-to-r from-chart-1 to-chart-3 px-5 text-white shadow-md shadow-chart-1/20 transition-all duration-300 hover:shadow-lg hover:shadow-chart-1/30 disabled:from-muted disabled:to-muted disabled:text-muted-foreground disabled:shadow-none"
                  >
                    <span className="absolute -top-0.5 left-3 text-[8px] opacity-70 transition-opacity group-hover/button:opacity-100">
                      *
                    </span>
                    <span className="absolute -bottom-0.5 right-4 text-[8px] opacity-70 transition-opacity group-hover/button:opacity-100">
                      *
                    </span>
                    {mutation.isPending ? (
                      <IconLoader2 className="size-4 animate-spin" />
                    ) : (
                      <IconDeviceFloppy className="size-4" />
                    )}
                    <span className="font-medium">
                      {mutation.isPending ? "保存中..." : "保存"}
                    </span>
                  </Button>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  )
}
