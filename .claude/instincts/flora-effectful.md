---
id: flora-effectful-patterns
trigger: "when writing Haskell code with effectful"
confidence: 0.9
domain: haskell
source: flora-server-analysis
---

# Flora Server Effectful Patterns

## Quick Reference

### Core Monad
```haskell
type MoeM es a = RequireCallStack => Eff es a
```

### Effect Stack for Web
```haskell
type MoeEff = Eff RouteEffects

type RouteEffects =
  '[ DB, Time, Log, Reader Env, Error ServerError, IOE ]
```

### Model Organization
```
Model/Entity/
├── Types.hs   -- Types, newtypes, Entity instances
├── Query.hs   -- SELECT operations (DB :> es)
├── Update.hs  -- INSERT/UPDATE/DELETE (DB :> es, Time :> es)
```

### Query Pattern
```haskell
getById :: DB :> es => EntityId -> Eff es (Maybe Entity)
getById eid = dbtToEff $ selectById (Only eid)
```

### Update Pattern
```haskell
update :: (DB :> es, Time :> es) => EntityId -> UpdateDTO -> Eff es ()
update eid dto = do
  ts <- Time.currentTime
  dbtToEff $ void $ execute updateQuery (dto, ts, eid)
```

### Custom Effect
```haskell
data MyEffect :: Effect where
  DoThing :: Arg -> MyEffect m Result

makeEffect ''MyEffect  -- generates doThing

runMyEffect :: IOE :> es => Eff (MyEffect : es) a -> Eff es a
```

### Deriving
```haskell
-- ID
newtype EntityId = EntityId UUID
  deriving (Eq, FromField, ToField, FromJSON, ToJSON) via UUID

-- Entity
data Entity = Entity { ... }
  deriving anyclass (FromRow, ToRow)
  deriving (Entity) via (GenericEntity '[TableName "entities"] Entity)
```
