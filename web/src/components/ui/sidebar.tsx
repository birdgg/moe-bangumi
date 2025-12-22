import * as React from "react";
import { cva, type VariantProps } from "class-variance-authority";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";
import { Separator } from "@/components/ui/separator";
import {
  IconChevronLeft,
  IconChevronRight,
  IconMenu2,
} from "@tabler/icons-react";

/* -----------------------------------------------------------------------------
 * Context
 * -------------------------------------------------------------------------- */

interface SidebarContextValue {
  collapsed: boolean;
  setCollapsed: (collapsed: boolean) => void;
  mobile: boolean;
  mobileOpen: boolean;
  setMobileOpen: (open: boolean) => void;
}

const SidebarContext = React.createContext<SidebarContextValue | null>(null);

function useSidebar() {
  const context = React.useContext(SidebarContext);
  if (!context) {
    throw new Error("useSidebar must be used within a SidebarProvider");
  }
  return context;
}

/* -----------------------------------------------------------------------------
 * Provider
 * -------------------------------------------------------------------------- */

interface SidebarProviderProps {
  children: React.ReactNode;
  defaultCollapsed?: boolean;
}

function SidebarProvider({
  children,
  defaultCollapsed = false,
}: SidebarProviderProps) {
  const [collapsed, setCollapsed] = React.useState(defaultCollapsed);
  const [mobileOpen, setMobileOpen] = React.useState(false);
  const [mobile, setMobile] = React.useState(false);

  React.useEffect(() => {
    const checkMobile = () => {
      setMobile(window.innerWidth < 768);
    };
    checkMobile();
    window.addEventListener("resize", checkMobile);
    return () => window.removeEventListener("resize", checkMobile);
  }, []);

  return (
    <SidebarContext.Provider
      value={{ collapsed, setCollapsed, mobile, mobileOpen, setMobileOpen }}
    >
      {children}
    </SidebarContext.Provider>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar
 * -------------------------------------------------------------------------- */

interface SidebarProps extends React.HTMLAttributes<HTMLElement> {
  side?: "left" | "right";
}

function Sidebar({
  className,
  side = "left",
  children,
  ...props
}: SidebarProps) {
  const { collapsed, mobile, mobileOpen, setMobileOpen } = useSidebar();

  if (mobile) {
    return (
      <>
        {/* Backdrop */}
        <div
          className={cn(
            "fixed inset-0 z-40 bg-background/80 backdrop-blur-sm transition-opacity duration-300",
            mobileOpen ? "opacity-100" : "pointer-events-none opacity-0",
          )}
          onClick={() => setMobileOpen(false)}
        />
        {/* Mobile sidebar */}
        <aside
          data-slot="sidebar"
          data-side={side}
          data-mobile="true"
          className={cn(
            "fixed top-0 z-50 flex h-full w-72 flex-col border-r border-sidebar-border bg-sidebar text-sidebar-foreground shadow-xl transition-transform duration-300 ease-out",
            side === "left" ? "left-0" : "right-0 border-l border-r-0",
            mobileOpen
              ? "translate-x-0"
              : side === "left"
                ? "-translate-x-full"
                : "translate-x-full",
            className,
          )}
          {...props}
        >
          {children}
        </aside>
      </>
    );
  }

  return (
    <aside
      data-slot="sidebar"
      data-side={side}
      data-collapsed={collapsed}
      className={cn(
        "group/sidebar relative flex h-full flex-col border-r border-sidebar-border bg-sidebar text-sidebar-foreground transition-[width] duration-300 ease-out",
        collapsed ? "w-16" : "w-64",
        side === "right" && "border-l border-r-0",
        className,
      )}
      {...props}
    >
      {children}
    </aside>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Header
 * -------------------------------------------------------------------------- */

function SidebarHeader({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      data-slot="sidebar-header"
      className={cn("flex h-14 shrink-0 items-center px-4", className)}
      {...props}
    >
      {children}
    </div>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Content
 * -------------------------------------------------------------------------- */

function SidebarContent({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      data-slot="sidebar-content"
      className={cn(
        "flex flex-1 flex-col gap-1 overflow-y-auto overflow-x-hidden px-3 py-2",
        className,
      )}
      {...props}
    >
      {children}
    </div>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Footer
 * -------------------------------------------------------------------------- */

function SidebarFooter({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      data-slot="sidebar-footer"
      className={cn(
        "mt-auto flex shrink-0 flex-col gap-1 border-t border-sidebar-border px-3 py-3",
        className,
      )}
      {...props}
    >
      {children}
    </div>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Group
 * -------------------------------------------------------------------------- */

function SidebarGroup({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      data-slot="sidebar-group"
      className={cn("flex flex-col gap-1", className)}
      {...props}
    >
      {children}
    </div>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Group Label
 * -------------------------------------------------------------------------- */

function SidebarGroupLabel({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  const { collapsed } = useSidebar();

  if (collapsed) return null;

  return (
    <div
      data-slot="sidebar-group-label"
      className={cn(
        "px-2 py-1.5 text-xs font-medium uppercase tracking-wider text-sidebar-foreground/50",
        className,
      )}
      {...props}
    >
      {children}
    </div>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Menu
 * -------------------------------------------------------------------------- */

function SidebarMenu({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLUListElement>) {
  return (
    <ul
      data-slot="sidebar-menu"
      className={cn("flex flex-col gap-0.5", className)}
      {...props}
    >
      {children}
    </ul>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Menu Item
 * -------------------------------------------------------------------------- */

function SidebarMenuItem({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLLIElement>) {
  return (
    <li
      data-slot="sidebar-menu-item"
      className={cn("relative", className)}
      {...props}
    >
      {children}
    </li>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Menu Button
 * -------------------------------------------------------------------------- */

const sidebarMenuButtonVariants = cva(
  "group/menu-button relative flex w-full items-center gap-3 rounded-lg px-3 py-2 text-sm font-medium transition-colors duration-150 outline-none focus-visible:ring-2 focus-visible:ring-sidebar-ring",
  {
    variants: {
      variant: {
        default:
          "text-sidebar-foreground/70 hover:bg-sidebar-accent hover:text-sidebar-accent-foreground",
        active:
          "bg-sidebar-primary text-sidebar-primary-foreground hover:bg-sidebar-primary/90",
      },
    },
    defaultVariants: {
      variant: "default",
    },
  },
);

interface SidebarMenuButtonProps
  extends
    React.ButtonHTMLAttributes<HTMLButtonElement>,
    VariantProps<typeof sidebarMenuButtonVariants> {
  asChild?: boolean;
  tooltip?: string;
}

function SidebarMenuButton({
  className,
  variant,
  children,
  tooltip,
  ...props
}: SidebarMenuButtonProps) {
  const { collapsed, mobile, setMobileOpen } = useSidebar();

  const handleClick = (e: React.MouseEvent<HTMLButtonElement>) => {
    if (mobile) {
      setMobileOpen(false);
    }
    props.onClick?.(e);
  };

  return (
    <button
      data-slot="sidebar-menu-button"
      className={cn(
        sidebarMenuButtonVariants({ variant }),
        collapsed && !mobile && "justify-center px-0",
        className,
      )}
      title={collapsed && !mobile ? tooltip : undefined}
      {...props}
      onClick={handleClick}
    >
      {children}
    </button>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Menu Button Icon
 * -------------------------------------------------------------------------- */

function SidebarMenuButtonIcon({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLSpanElement>) {
  return (
    <span
      data-slot="sidebar-menu-button-icon"
      className={cn(
        "flex size-5 shrink-0 items-center justify-center [&_svg]:size-5",
        className,
      )}
      {...props}
    >
      {children}
    </span>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Menu Button Label
 * -------------------------------------------------------------------------- */

function SidebarMenuButtonLabel({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLSpanElement>) {
  const { collapsed, mobile } = useSidebar();

  if (collapsed && !mobile) return null;

  return (
    <span
      data-slot="sidebar-menu-button-label"
      className={cn("truncate", className)}
      {...props}
    >
      {children}
    </span>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Toggle
 * -------------------------------------------------------------------------- */

function SidebarToggle({
  className,
  ...props
}: React.HTMLAttributes<HTMLButtonElement>) {
  const { collapsed, setCollapsed, mobile } = useSidebar();

  if (mobile) return null;

  return (
    <Button
      data-slot="sidebar-toggle"
      variant="ghost"
      size="icon-sm"
      className={cn(
        "shrink-0 transition-all",
        collapsed ? "" : "ml-auto",
        className,
      )}
      onClick={() => setCollapsed(!collapsed)}
      {...props}
    >
      {collapsed ? <IconChevronRight /> : <IconChevronLeft />}
      <span className="sr-only">Toggle sidebar</span>
    </Button>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Mobile Trigger
 * -------------------------------------------------------------------------- */

function SidebarMobileTrigger({
  className,
  ...props
}: React.HTMLAttributes<HTMLButtonElement>) {
  const { mobile, setMobileOpen } = useSidebar();

  if (!mobile) return null;

  return (
    <Button
      data-slot="sidebar-mobile-trigger"
      variant="ghost"
      size="icon"
      className={cn(className)}
      onClick={() => setMobileOpen(true)}
      {...props}
    >
      <IconMenu2 />
      <span className="sr-only">Open sidebar</span>
    </Button>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Inset
 * -------------------------------------------------------------------------- */

function SidebarInset({
  className,
  children,
  ...props
}: React.HTMLAttributes<HTMLDivElement>) {
  return (
    <div
      data-slot="sidebar-inset"
      className={cn("flex flex-1 flex-col overflow-hidden", className)}
      {...props}
    >
      {children}
    </div>
  );
}

/* -----------------------------------------------------------------------------
 * Sidebar Separator
 * -------------------------------------------------------------------------- */

function SidebarSeparator({
  className,
  ...props
}: React.ComponentProps<typeof Separator>) {
  return (
    <Separator
      data-slot="sidebar-separator"
      className={cn("my-2 bg-sidebar-border", className)}
      {...props}
    />
  );
}

export {
  Sidebar,
  SidebarContent,
  SidebarFooter,
  SidebarGroup,
  SidebarGroupLabel,
  SidebarHeader,
  SidebarInset,
  SidebarMenu,
  SidebarMenuButton,
  SidebarMenuButtonIcon,
  SidebarMenuButtonLabel,
  SidebarMenuItem,
  SidebarMobileTrigger,
  SidebarProvider,
  SidebarSeparator,
  SidebarToggle,
  useSidebar,
};
