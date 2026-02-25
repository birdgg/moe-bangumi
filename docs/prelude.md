# Moe.Prelude

`Moe.Prelude` re-exports all commonly used `effectful` modules so most files only need `import Moe.Prelude`.

## Hiding list

Some files need `import Moe.Prelude hiding (...)` due to naming conflicts with non-effectful imports.

| Symbol | Conflicts with | Files |
|--------|---------------|-------|
| `(:>)` | `Servant.(:>)` (route combinator) | `Web/API/Routes.hs`, `Infra/Metadata/BangumiData/API.hs` |
| `type (:.)(..)` | `Servant.(:.)` (context combinator) | `Web/Server.hs` |
| `try` | `Control.Exception.try` / `Control.Exception.Safe.try` (pure IO) | `Infra/Rss/Client.hs`, `Infra/Notification/Client.hs`, `Infra/Metadata/Mikan.hs`, `Infra/Update/Adapter.hs` |
| `doesFileExist` | `System.Directory.doesFileExist` (pure IO) | `Web/API/Log/Handler.hs`, `Infra/Update/Adapter.hs`, `test/.../CollectionSpec.hs` |
| `try`, `threadDelay`, `bracket_` | `Control.Exception` / `Control.Concurrent` (pure IO supervisor) | `app/Supervisor.hs` |

## Qualified imports still needed

These cannot be covered by Prelude re-exports:

| Import | Reason | Files (~count) |
|--------|--------|----------------|
| `Effectful.Log qualified as Log` | Qualified calls like `Log.logInfo_` | ~15 |
| `Effectful.Concurrent.STM qualified as STM` | Qualified calls like `STM.atomically` | ~7 |
| `Effectful.TH (makeEffect)` | Template Haskell for effect generation | ~6 |
| `Effectful.Dispatch.Static ()` | Instance-only import | 1 (`Web/Server.hs`) |
| `Effectful.FileSystem.IO.ByteString.Lazy qualified as LBS` | Qualified lazy ByteString IO | 1 |
| `Effectful.Concurrent.QSem` | Semaphore (not common enough) | 1 (`App/Calendar.hs`) |
| `Effectful.Sqlite (Migration, ...)` | Bootstrap-only migration utilities | 1 (`App/Bootstrap.hs`) |
