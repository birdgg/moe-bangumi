import { createRootRoute, Outlet } from "@tanstack/react-router";
import { TanStackRouterDevtools } from "@tanstack/react-router-devtools";
import { AppLayout } from "@/components/app-layout";

export const Route = createRootRoute({
  component: RootLayout,
});

function RootLayout() {
  return (
    <AppLayout>
      <Outlet />
      <TanStackRouterDevtools />
    </AppLayout>
  );
}
