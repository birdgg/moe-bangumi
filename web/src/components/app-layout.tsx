import * as React from "react";
import { useState, useEffect, useCallback } from "react";
import { SidebarInset, SidebarProvider } from "@/components/ui/sidebar";
import { ThemeColorSelector } from "@/components/theme-color-selector";
import { ThemeToggleButton } from "@/components/theme-toggle-button";
import { AppSidebar } from "@/components/app-sidebar";
import { RssSearchModal } from "@/features/rss-search/components/rss-search-modal";
import { IconSearch } from "@tabler/icons-react";

interface AppLayoutProps {
  children: React.ReactNode;
}

export function AppLayout({ children }: AppLayoutProps) {
  const [searchOpen, setSearchOpen] = useState(false);

  const handleGlobalKeyDown = useCallback(
    (e: KeyboardEvent) => {
      if ((e.metaKey || e.ctrlKey) && e.key === "k") {
        e.preventDefault();
        setSearchOpen((prev) => !prev);
      }
    },
    [],
  );

  useEffect(() => {
    window.addEventListener("keydown", handleGlobalKeyDown);
    return () => window.removeEventListener("keydown", handleGlobalKeyDown);
  }, [handleGlobalKeyDown]);

  return (
    <SidebarProvider style={{ "--sidebar-width": "14rem" } as React.CSSProperties}>
      <AppSidebar />

      <SidebarInset className="bg-background">
        <header className="glass-header relative flex h-14 shrink-0 items-center justify-end px-4">
          <div className="flex items-center gap-2">
            <button
              onClick={() => setSearchOpen(true)}
              className="glass-pill group inline-flex items-center gap-2 rounded-lg px-2 py-1.5 bg-foreground/[0.03] text-foreground/40 transition-all hover:bg-foreground/[0.06] hover:text-foreground/60"
              aria-label="Search RSS"
            >
              <IconSearch className="size-3.5" />
              <span className="text-[11px] transition-colors hidden sm:inline">
                Search
              </span>
              <kbd className="glass-kbd hidden sm:inline-flex items-center gap-0.5 rounded px-1 py-0.5 text-[10px] font-mono transition-colors">
                <span className="text-[9px]">&#8984;</span>K
              </kbd>
            </button>
            <ThemeColorSelector />
            <ThemeToggleButton />
          </div>
        </header>

        <main className="flex-1 overflow-auto">{children}</main>
      </SidebarInset>

      <RssSearchModal open={searchOpen} onOpenChange={setSearchOpen} />
    </SidebarProvider>
  );
}
