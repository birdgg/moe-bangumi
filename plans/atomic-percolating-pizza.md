# Setup TanStack Router in web/

## 概述

在 React 19 + TypeScript + Vite 项目中配置 TanStack Router，使用基于文件的路由（file-based routing）。

## 需要安装的依赖

```bash
cd web && bun add @tanstack/react-router @tanstack/router-plugin
```

可选（开发工具）：
```bash
bun add -D @tanstack/react-router-devtools
```

## 修改的文件

### 1. `web/vite.config.ts`

添加 TanStack Router 插件（必须在 react 插件之前）：

```ts
import path from "path"
import tailwindcss from "@tailwindcss/vite"
import react from "@vitejs/plugin-react"
import { tanstackRouter } from "@tanstack/router-plugin/vite"
import { defineConfig } from "vite"

export default defineConfig({
  plugins: [
    tanstackRouter({
      target: "react",
      autoCodeSplitting: true,
    }),
    react(),
    tailwindcss(),
  ],
  resolve: {
    alias: {
      "@": path.resolve(__dirname, "./src"),
    },
  },
})
```

### 2. 创建 `web/src/routes/__root.tsx`

根路由，包含全局布局：

```tsx
import { Outlet, createRootRoute } from "@tanstack/react-router"

export const Route = createRootRoute({
  component: RootComponent,
})

function RootComponent() {
  return <Outlet />
}
```

### 3. 创建 `web/src/routes/index.tsx`

首页路由（`/`）：

```tsx
import { createFileRoute } from "@tanstack/react-router"
import { ComponentExample } from "@/components/component-example"

export const Route = createFileRoute("/")({
  component: IndexComponent,
})

function IndexComponent() {
  return <ComponentExample />
}
```

### 4. 更新 `web/src/main.tsx`

使用 RouterProvider 替代 App 组件：

```tsx
import { StrictMode } from "react"
import ReactDOM from "react-dom/client"
import { RouterProvider, createRouter } from "@tanstack/react-router"
import { routeTree } from "./routeTree.gen"
import "./index.css"

const router = createRouter({ routeTree })

declare module "@tanstack/react-router" {
  interface Register {
    router: typeof router
  }
}

const rootElement = document.getElementById("root")!
ReactDOM.createRoot(rootElement).render(
  <StrictMode>
    <RouterProvider router={router} />
  </StrictMode>
)
```

### 5. 删除 `web/src/App.tsx`

不再需要，路由系统接管入口。

## 文件结构变化

```
web/src/
├── main.tsx              # 更新：使用 RouterProvider
├── index.css
├── routeTree.gen.ts      # 自动生成（不要手动编辑）
├── routes/               # 新建目录
│   ├── __root.tsx        # 根路由/布局
│   └── index.tsx         # 首页 (/)
├── components/
│   └── ...
└── lib/
    └── ...
```

## 验证步骤

1. 安装依赖：`cd web && bun install`
2. 启动开发服务器：`bun run dev`
3. 访问 `http://localhost:5173/`，确认页面正常显示
4. 检查控制台无错误
5. 确认 `routeTree.gen.ts` 已自动生成
