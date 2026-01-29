# moe-bangumi 架构概览

> 最后更新: 2026-01-30

## 技术栈

| 组件 | 技术 |
|------|------|
| 语言 | Haskell (GHC 9.12.2, GHC2024) |
| Prelude | Relude |
| Effect 系统 | Effectful |
| 数据库 | SQLite (effectful-sqlite) |
| 日志 | log-base, log-effectful |
| Web | Servant |
| 测试 | Tasty + HUnit |

## 三层架构

```
+-------------------------------------------------------------+
|                        App Layer                             |
|  Moe.App.*  (运行时环境、Effect 组合、程序入口)                |
+-------------------------------------------------------------+
|                      Adapter Layer                           |
|  Moe.Adapter.*  (Effect 解释器，run* 前缀)                   |
+-------------------------------------------------------------+
|                       Infra Layer                            |
|  Moe.Infra.*  (三方 API 封装，纯 IO)                         |
+-------------------------------------------------------------+
|                      Effect Layer                            |
|  Moe.Effect.*  (Effect 接口定义)                             |
+-------------------------------------------------------------+
|                      Domain Layer                            |
|  Moe.Domain.*  (纯业务逻辑、领域模型、解析器)                  |
+-------------------------------------------------------------+
```

## 依赖规则

```
Domain  <-----  Effect  <-----  Adapter
                  ^                |
                  +------ Infra ---+
                             ^
              App -----------+
```

| 层级 | 可依赖 | 禁止依赖 |
|------|--------|----------|
| Domain | 无 | Effect, Adapter, Infra, App |
| Effect | Domain | Adapter, App |
| Infra | Domain | Effect, Adapter, App |
| Adapter | Domain, Effect, Infra | App |
| App | 所有层 | - |

## 入口点

| 入口 | 模块 | 描述 |
|------|------|------|
| CLI | `app/cli/Main.hs` | 命令行工具 |
| Web | `Moe.Web.Server` | HTTP API 服务 |

## Effect 组合

```haskell
type MoeEffects =
  '[ BangumiQuery,
     BangumiUpdate,
     BangumiData,
     Metadata,
     Log,
     SQLite,
     Error MoeError,
     IOE
   ]

type MoeM a = Eff MoeEffects a
```

## 目录结构

```
src/Moe/
+-- Domain/       # 纯业务逻辑（无 Effect）
+-- Effect/       # Effect 接口定义
+-- Adapter/      # Effect 解释器
+-- Infra/        # 三方 API 封装
+-- App/          # 应用层（Env, Error, Monad）
+-- Web/          # Web 服务
```

## 参考

- [Three Layer Haskell Cake](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html)
- [Effectful](https://hackage.haskell.org/package/effectful)
