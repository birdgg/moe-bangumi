import * as React from "react";
import { SidebarInset, SidebarProvider } from "@/components/ui/sidebar";
import { ThemeColorSelector } from "@/components/theme-color-selector";
import { ThemeToggleButton } from "@/components/theme-toggle-button";
import { AppSidebar } from "@/components/app-sidebar";

interface AppLayoutProps {
  children: React.ReactNode;
}

export function AppLayout({ children }: AppLayoutProps) {
  return (
    <SidebarProvider style={{ "--sidebar-width": "14rem" } as React.CSSProperties}>
      <AppSidebar />

      <SidebarInset className="bg-background">
        <header className="relative flex h-14 shrink-0 items-center justify-end border-b border-border/50 px-4">
          <div className="absolute inset-x-0 top-0 h-px bg-linear-to-r from-transparent via-chart-2/30 to-transparent" />

          <div className="flex items-center gap-2">
            <ThemeColorSelector />
            <ThemeToggleButton />
          </div>
        </header>

        <main className="flex-1 overflow-auto">{children}</main>
      </SidebarInset>
    </SidebarProvider>
  );
}
