import { createFileRoute, useNavigate, useSearch } from "@tanstack/react-router";
import { z } from "zod";
import { toast } from "sonner";
import { Button } from "@/components/ui/button";
import { IconDeviceFloppy } from "@tabler/icons-react";
import {
  GeneralSection,
  DownloaderSection,
  FilterSection,
  ProxySection,
  NotificationSection,
  PrioritySection,
  MediaLibrarySection,
  SystemSection,
  SettingsSidebar,
  type SettingsSection,
} from "@/features/settings/components";
import { useSettingsForm } from "@/features/settings/hooks";

const searchSchema = z.object({
  section: z.enum([
    "general",
    "downloader",
    "filter",
    "proxy",
    "notification",
    "priority",
    "media-library",
    "system",
  ]).optional().default("general"),
});

export const Route = createFileRoute("/settings")({
  validateSearch: searchSchema,
  component: SettingsPage,
});

function SettingsPage() {
  const { section: activeSection } = useSearch({ from: "/settings" });
  const navigate = useNavigate({ from: "/settings" });
  const form = useSettingsForm();

  const setActiveSection = (section: SettingsSection) => {
    navigate({ search: { section } });
  };

  const handleSave = () => {
    toast.info("保存功能暂未实现", {
      description: "设置将在后端 API 接入后可用",
    });
  };

  const renderContent = () => {
    switch (activeSection) {
      case "general":
        return <GeneralSection form={form} />;
      case "downloader":
        return <DownloaderSection form={form} />;
      case "priority":
        return <PrioritySection form={form} />;
      case "filter":
        return <FilterSection form={form} />;
      case "proxy":
        return <ProxySection form={form} />;
      case "notification":
        return <NotificationSection form={form} />;
      case "media-library":
        return <MediaLibrarySection form={form} />;
      case "system":
        return <SystemSection />;
      default:
        return <GeneralSection form={form} />;
    }
  };

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
                  e.preventDefault();
                  e.stopPropagation();
                  handleSave();
                }}
              >
                <div className="rounded-2xl border bg-card p-6">
                  {renderContent()}
                </div>

                <div className="flex justify-end">
                  <Button
                    type="submit"
                    className="cursor-pointer relative gap-2 overflow-hidden bg-linear-to-r from-chart-1 to-chart-3 px-5 text-white shadow-md shadow-chart-1/20 transition-all duration-300 hover:shadow-lg hover:shadow-chart-1/30 disabled:from-muted disabled:to-muted disabled:text-muted-foreground disabled:shadow-none"
                  >
                    <span className="absolute -top-0.5 left-3 text-[8px] opacity-70 transition-opacity group-hover/button:opacity-100">
                      *
                    </span>
                    <span className="absolute -bottom-0.5 right-4 text-[8px] opacity-70 transition-opacity group-hover/button:opacity-100">
                      *
                    </span>
                    <IconDeviceFloppy className="size-4" />
                    <span className="font-medium">保存</span>
                  </Button>
                </div>
              </form>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
