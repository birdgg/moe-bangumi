---
name: api
description: Create an agent team for full-stack feature development. One agent writes the Haskell Servant backend API, another builds the React frontend UI consuming that API. Use when asked to "build a feature", "add a new page", "create an endpoint with UI", or "fullstack feature".
---

# Full-Stack Feature Team

Spawn a coordinated agent team for implementing full-stack features across the Haskell backend and React frontend.

## Team Structure

Two agents working in coordination:

1. **backend-api** - Writes Haskell Servant API (routes, handlers, DTOs, domain logic)
2. **frontend-ui** - Builds React UI consuming the API (components, hooks, routes)

## Workflow

When invoked with a feature description:

### Step 1: Create Team

Use TeamCreate to create a team named `fullstack-{feature-slug}`.

### Step 2: Create Tasks

Create tasks for the feature, frontend task first create essential ui, and wait for backend to complete before api dependent work.:

1. **Backend tasks** (assigned to backend-api):
   - Define domain types if needed (in `src/Moe/Domain/`)
   - Create DTO types (in `src/Moe/Web/API/DTO/`)
   - Add Servant routes (in `src/Moe/Web/API/Routes.hs`)
   - Implement handlers (in `src/Moe/Web/API/{Feature}/Handler.hs`)
   - Wire into server (in `src/Moe/Web/API/Server.hs`)
   - Build and verify with `cabal build`

2. **Frontend tasks** (assigned to frontend-ui, blocked by backend completion):
   - Build UI components with shadcn/ui + Tailwind (in `web/src/features/{feature}/components/`)
   - Regenerate API client with `cd web && bun run gen:api`
   - Create hooks using `@tanstack/react-query` (in `web/src/features/{feature}/hooks/`)
   - Add route if needed (in `web/src/routes/`)
   - Verify with `cd web && bun run build`

### Step 3: Spawn Agents

Launch two agents via Task tool with the team_name parameter:

#### backend-api agent

```
subagent_type: general-purpose
name: backend-api
team_name: fullstack-{feature-slug}
```

Prompt should include:
- The feature requirement
- Backend architecture context (below)
- Assigned task IDs

#### frontend-ui agent

```
subagent_type: general-purpose
name: frontend-ui
team_name: fullstack-{feature-slug}
```

Prompt should include:
- The feature requirement
- Frontend architecture context (below)
- Assigned task IDs
- Note: wait for backend tasks to complete before starting API-dependent work

### Step 4: Coordinate

Monitor task progress. When backend completes, send message to frontend-ui to proceed. When all tasks complete, shut down the team.

---

## Backend Architecture Context

Provide this to the backend-api agent:

```
Project: moe-bangumi (Haskell, GHC 9.14.1, GHC2024)
Prelude: Relude via Moe.Prelude
Effect system: Effectful

Key patterns:
- Routes: Servant NamedRoutes in src/Moe/Web/API/Routes.hs
- DTOs: src/Moe/Web/API/DTO/{Feature}.hs with toXxxResponse converters
- Handlers: src/Moe/Web/API/{Feature}/Handler.hs
- Handler type: ServerEff (defined in Moe.Web.Types)
- Domain types: src/Moe/Domain/
- Database: Effectful.Sqlite, queries in src/Moe/Infra/Database/
- Server wiring: src/Moe/Web/API/Server.hs

Handler pattern:
  handleXxx :: Param -> ServerEff ResponseType
  handleXxx param = do
    result <- someEffect param
    pure $ toResponse result

Route pattern (add field to Routes' record):
  newRoute ::
    mode
      :- "path"
        :> QueryParam' '[Required, Strict] "param" ParamType
        :> Get '[JSON] ResponseType

DTO pattern:
  data XxxResponse = XxxResponse { field :: Type }
    deriving stock (Generic, Show)
    deriving anyclass (ToJSON)

Build command: cabal build
Style: simple haddock, use "bangumi" not "anime", no field suffixes, run hlint after edits
```

## Frontend Architecture Context

Provide this to the frontend-ui agent:

```
Project: moe-bangumi frontend (React 19, TypeScript)
Router: @tanstack/react-router
State: @tanstack/react-query
UI: shadcn/ui + Tailwind CSS 4 + @base-ui/react
Icons: @tabler/icons-react
Animation: framer-motion
Build: Vite 7, Bun

Key patterns:
- API client: auto-generated in web/src/client/ from OpenAPI spec
  - Regenerate: cd web && bun run gen:api
  - Import from @/client/@tanstack/react-query.gen for query options
  - Import from @/client/sdk.gen for direct API calls
- Hooks: web/src/features/{feature}/hooks/use-{feature}.ts
  - Use useQuery with generated options: useQuery({ ...getApiXxxOptions({ query: params }) })
- Components: web/src/features/{feature}/components/
- Routes: web/src/routes/{route-name}.tsx
  - Use createFileRoute for new routes
- Shared components: web/src/components/ (shadcn/ui based)
- Layout: AppLayout with sidebar navigation

Route pattern:
  import { createFileRoute } from "@tanstack/react-router"
  export const Route = createFileRoute("/path")({ component: PageComponent })

Hook pattern:
  export function useFeature(params) {
    return useQuery({ ...getApiFeatureOptions({ query: params }) })
  }

Build command: cd web && bun run build
Style: functional components, no class components
```

## Important Notes

- Backend agent should build and verify before frontend starts API integration
- Frontend agent can start on non-API-dependent UI work (layout, static components) while waiting
- Both agents should follow project CLAUDE.md conventions
- Backend: run hlint on changed files
- Frontend: use existing shadcn components where possible
- Do not write tests unless explicitly requested
