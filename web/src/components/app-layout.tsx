import * as React from "react";
import { SidebarInset, SidebarProvider } from "@/components/ui/sidebar";
import { Button } from "@/components/ui/button";
import { IconPlus, IconRefresh } from "@tabler/icons-react";
import { SearchBangumiModal, BangumiModal } from "@/features/bangumi/components";
import { type SearchedMetadata, scanImportMutation, getBangumiQueryKey } from "@/lib/api";
import { ThemeColorSelector } from "@/components/theme-color-selector";
import { ThemeToggleButton } from "@/components/theme-toggle-button";
import { AppSidebar } from "@/components/app-sidebar";
import { subjectToModalData } from "@/lib/converters";
import { useMutation, useQueryClient } from "@tanstack/react-query";
import { toast } from "sonner";

interface AppLayoutProps {
  children: React.ReactNode;
}

interface AddBangumiButtonProps {
  onClick: () => void;
}

function AddBangumiButton({ onClick }: AddBangumiButtonProps) {
  return (
    <Button
      className="group relative gap-2 overflow-hidden rounded-xl bg-linear-to-r from-chart-1 via-chart-2 to-chart-3 px-4 py-2 text-white shadow-lg shadow-chart-3/30 transition-all duration-300 hover:scale-105 hover:shadow-xl hover:shadow-chart-2/40"
      onClick={onClick}
    >
      <span className="absolute inset-0 bg-linear-to-r from-chart-2 via-chart-3 to-chart-1 opacity-0 transition-opacity duration-500 group-hover:opacity-100" />
      <IconPlus className="relative z-10 size-4 transition-transform duration-300 group-hover:rotate-90" />
      <span className="relative z-10 hidden font-medium sm:inline">添加番剧</span>
    </Button>
  );
}

function SyncButton() {
  const queryClient = useQueryClient();
  const mutation = useMutation({
    ...scanImportMutation(),
    onSuccess: () => {
      toast.success("扫描已启动", {
        description: "正在后台扫描本地目录，完成后自动刷新...",
      });
      // Delay refresh since scan runs in background
      setTimeout(() => {
        queryClient.invalidateQueries({ queryKey: getBangumiQueryKey() });
      }, 5000);
    },
    onError: (error) => {
      const message = error.message || "扫描启动失败";
      if (message.includes("already in progress")) {
        toast.warning("扫描进行中", {
          description: "请等待当前扫描完成",
        });
      } else {
        toast.error("扫描失败", {
          description: message,
        });
      }
    },
  });

  return (
    <Button
      variant="ghost"
      size="icon"
      className="group size-9 rounded-xl transition-all duration-300 hover:scale-105 hover:bg-transparent"
      onClick={() => mutation.mutate({ body: {} })}
      disabled={mutation.isPending}
      title="扫描本地目录"
    >
      <IconRefresh
        className={`size-4 text-foreground transition-transform duration-500 ${
          mutation.isPending ? "animate-spin" : "group-hover:rotate-180"
        }`}
      />
    </Button>
  );
}

export function AppLayout({ children }: AppLayoutProps) {
  const [searchModalOpen, setSearchModalOpen] = React.useState(false);
  const [addModalOpen, setAddModalOpen] = React.useState(false);
  const [selectedSubject, setSelectedSubject] = React.useState<SearchedMetadata | null>(null);

  const handleSelectBangumi = (subject: SearchedMetadata) => {
    setSelectedSubject(subject);
    setAddModalOpen(true);
  };

  const handleAddSuccess = () => {
    setSelectedSubject(null);
  };

  return (
    <SidebarProvider style={{ "--sidebar-width": "14rem" } as React.CSSProperties}>
      <AppSidebar />

      <SidebarInset className="bg-background">
        <header className="relative flex h-14 shrink-0 items-center justify-end border-b border-border/50 px-4">
          <div className="absolute inset-x-0 top-0 h-px bg-linear-to-r from-transparent via-chart-2/30 to-transparent" />

          <div className="flex items-center gap-2">
            <AddBangumiButton onClick={() => setSearchModalOpen(true)} />
            <SyncButton />
            <ThemeColorSelector />
            <ThemeToggleButton />
          </div>
        </header>

        <SearchBangumiModal
          open={searchModalOpen}
          onOpenChange={setSearchModalOpen}
          onSelect={handleSelectBangumi}
        />

        {selectedSubject && (
          <BangumiModal
            open={addModalOpen}
            onOpenChange={setAddModalOpen}
            mode="add"
            data={subjectToModalData(selectedSubject)}
            onSuccess={handleAddSuccess}
          />
        )}

        <main className="flex-1 overflow-auto">{children}</main>
      </SidebarInset>
    </SidebarProvider>
  );
}
