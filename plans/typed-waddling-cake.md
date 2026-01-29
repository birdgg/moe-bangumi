# Web Server 配置计划

## 目标
从 moe-bangumi-backup 拷贝 web server 基础配置，API 留空但建好目录结构。

## 目录结构

```
src/
└── Web/
    ├── Server.hs       # 主服务器启动（简化版）
    ├── Routers.hs      # 路由定义
    ├── Types.hs        # Effect 类型
    ├── Scalar.hs       # API 文档 UI
    └── API/
        ├── Routes.hs   # API 路由（空占位符）
        ├── Server.hs   # API 服务器（空占位符）
        ├── Routes/     # 各 API 路由定义目录（空）
        └── Server/     # 各 API 实现目录（空）

app/
└── Main.hs             # 更新为调用 Web.Server.runMoe
```

## 实现步骤

### 1. 更新 moe-bangumi.cabal

**添加依赖**：
```
http-media
http-types
openapi3
optics-core
servant
servant-openapi3
servant-server
wai
wai-app-static
warp
```

**添加暴露模块**：
```
Web.Server
Web.Routers
Web.Types
Web.Scalar
Web.API.Routes
Web.API.Server
```

### 2. 创建 src/Web/Scalar.hs
直接从 backup 拷贝，包含：
- `HTML` 类型
- `RawHtml` 类型
- `scalarHtml` 函数（生成 Scalar API 文档页面）

### 3. 创建 src/Web/Types.hs
简化版 Effect 栈（目前只用基础 Effect）：
```haskell
type MoeEff = Eff RouteEffects
type RouteEffects =
  '[ Error ServerError
   , IOE
   ]
```

### 4. 创建 src/Web/API/Routes.hs
空 API 路由：
```haskell
type Routes = "api" :> NamedRoutes Routes'
data Routes' mode = Routes'
  deriving stock (Generic)
```

### 5. 创建 src/Web/API/Server.hs
空 API 服务器：
```haskell
apiServer :: ServerT API.Routes MoeEff
apiServer = API.Routes'
```

### 6. 创建 src/Web/Routers.hs
从 backup 拷贝，定义：
- `ServerRoutes` 类型
- `Routes` 数据类型（api, doc, assets）
- `DocRoutes` 数据类型

### 7. 创建 src/Web/Server.hs
简化版服务器（不含后台任务）：
- `runMoe :: IO ()` - 启动 Warp 服务器
- `mkServer` - 创建 WAI Application
- `moeServer` - 定义路由处理
- `spaFileServer` - SPA 静态文件服务
- `naturalTransform` - Effect 栈转换
- `openApiHandler` - OpenAPI 文档

### 8. 更新 app/Main.hs
```haskell
module Main (main) where

import Web.Server (runMoe)

main :: IO ()
main = runMoe
```

### 9. 创建空目录
- `src/Web/API/Routes/` (空目录，后续添加各 API 路由)
- `src/Web/API/Server/` (空目录，后续添加各 API 实现)

## 关键文件修改

| 文件 | 操作 |
|------|------|
| `moe-bangumi.cabal` | 添加依赖和模块 |
| `src/Web/Server.hs` | 新建 |
| `src/Web/Routers.hs` | 新建 |
| `src/Web/Types.hs` | 新建 |
| `src/Web/Scalar.hs` | 新建 |
| `src/Web/API/Routes.hs` | 新建（空） |
| `src/Web/API/Server.hs` | 新建（空） |
| `app/Main.hs` | 修改 |

## 验证

```bash
cabal build
cabal run moe-bangumi
# 访问 http://localhost:3000/docs/ 看到 API 文档页面
# 访问 http://localhost:3000/ 看到前端页面（需要先 cd web && bun run build）
```
