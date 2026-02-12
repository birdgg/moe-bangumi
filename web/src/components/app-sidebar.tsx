import { motion } from "framer-motion";
import {
  Sidebar,
  SidebarContent,
  SidebarGroup,
  SidebarMenu,
  SidebarMenuItem,
  useSidebar,
} from "@/components/ui/sidebar";
import {
  Tooltip,
  TooltipContent,
  TooltipTrigger,
} from "@/components/ui/tooltip";
import {
  IconDeviceTv,
  IconCalendarWeek,
  IconFileText,
  IconSettings,
} from "@tabler/icons-react";
import { useLocation, useNavigate } from "@tanstack/react-router";
import { cn } from "@/lib/utils";

interface SidebarItem {
  id: string;
  label: string;
  icon: React.ComponentType<{ className?: string }>;
  path: "/" | "/settings" | "/schedule" | "/logs";
}

const sidebarItems: SidebarItem[] = [
  { id: "anime", label: "动漫", icon: IconDeviceTv, path: "/" },
  {
    id: "schedule",
    label: "每日放送",
    icon: IconCalendarWeek,
    path: "/schedule",
  },
  { id: "logs", label: "日志", icon: IconFileText, path: "/logs" },
  { id: "settings", label: "设置", icon: IconSettings, path: "/settings" },
];

function NavItem({
  item,
  isActive,
  onClick,
  index,
}: {
  item: SidebarItem;
  isActive: boolean;
  onClick: () => void;
  index: number;
}) {
  const { state } = useSidebar();
  const Icon = item.icon;

  return (
    <motion.div
      initial={{ opacity: 0, x: -12 }}
      animate={{ opacity: 1, x: 0 }}
      transition={{ duration: 0.35, delay: index * 0.06, ease: "easeOut" }}
    >
      <SidebarMenuItem>
        <Tooltip>
          <TooltipTrigger
            render={
              <button
                onClick={onClick}
                data-active={isActive || undefined}
                className={cn(
                  "relative flex w-full items-center gap-3 rounded-xl px-3 py-2.5 text-sm font-medium",
                  "transition-all duration-200 outline-none overflow-hidden",
                  "focus-visible:ring-2 focus-visible:ring-foreground/10",
                  "group-data-[collapsible=icon]:size-8! group-data-[collapsible=icon]:rounded-lg group-data-[collapsible=icon]:p-0! group-data-[collapsible=icon]:justify-center",
                  isActive
                    ? "text-foreground"
                    : "text-foreground/40 hover:text-foreground/70 hover:bg-foreground/[0.04]",
                )}
              />
            }
          >
            {/* Glass active background */}
            {isActive && (
              <motion.div
                layoutId="sidebar-active-bg"
                className="absolute inset-0 rounded-xl glass-nav-active"
                style={{ borderRadius: 12 }}
                transition={{ type: "spring", stiffness: 380, damping: 30 }}
              />
            )}

            {/* Active left pill indicator */}
            {isActive && (
              <motion.div
                layoutId="sidebar-active-pill"
                className="absolute top-1/2 left-0 h-5 w-1 -translate-y-1/2 rounded-r-full bg-chart-1"
                transition={{ type: "spring", stiffness: 380, damping: 30 }}
              />
            )}

            <div className="relative z-10 flex shrink-0 items-center justify-center">
              <Icon className="size-[18px]" />
            </div>

            <span className="relative z-10 truncate group-data-[collapsible=icon]:hidden">
              {item.label}
            </span>
          </TooltipTrigger>

          <TooltipContent
            side="right"
            align="center"
            hidden={state !== "collapsed"}
          >
            {item.label}
          </TooltipContent>
        </Tooltip>
      </SidebarMenuItem>
    </motion.div>
  );
}

export function AppSidebar() {
  const location = useLocation();
  const navigate = useNavigate();

  const getActiveItem = () => {
    if (location.pathname === "/settings") return "settings";
    if (location.pathname === "/schedule") return "schedule";
    if (location.pathname === "/logs") return "logs";
    return "anime";
  };
  const activeItem = getActiveItem();

  return (
    <Sidebar collapsible="icon" className="border-r-0 glass-sidebar">
      <SidebarContent className="relative z-10 pt-4">
        <SidebarGroup className="px-2">
          <SidebarMenu className="gap-1">
            {sidebarItems.map((item, index) => (
              <NavItem
                key={item.id}
                item={item}
                isActive={activeItem === item.id}
                onClick={() => navigate({ to: item.path })}
                index={index}
              />
            ))}
          </SidebarMenu>
        </SidebarGroup>
      </SidebarContent>
    </Sidebar>
  );
}
