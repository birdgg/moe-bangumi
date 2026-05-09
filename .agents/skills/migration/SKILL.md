---
name: migration
description: Create a new SQLite migration file and validate database queries. Use when asked to "add a column", "create migration", "modify schema", "add database field", or "alter table".
---

# Database Migration

Create a new SQLite migration and ensure all database queries stay in sync.

## Workflow

### Step 1: Create Migration File

Generate a timestamped migration file in `migrations/`:

```bash
# Naming convention: YYYYMMDDHHMMSS_description.sql
# Example: 20260215000000_add_first_air_year.sql
```

Use the current date/time for the timestamp. Description should be snake_case and concise.

### Step 2: Write SQL

Write the migration SQL. Follow existing patterns:
- Use `ALTER TABLE ... ADD COLUMN` for new columns
- Use `CREATE TABLE IF NOT EXISTS` for new tables
- Keep migrations atomic - one logical change per file

### Step 3: Validate Database Queries (CRITICAL)

After creating the migration, you MUST scan and update all affected queries:

1. **Check `src/Moe/Infra/Database/Bangumi.hs`**:
   - Update `bangumiColumnList` if bangumi table was modified
   - This propagates to `bangumiColumns` and `bangumiColumnsAs` automatically
   - Check `createBangumi`, `updateBangumi`, `upsertByBgmtvId` for INSERT/UPDATE column lists

2. **Check `src/Moe/Infra/Database/Tracking.hs`**:
   - JOIN queries that reference bangumi columns must use `bangumiColumnsAs`
   - Verify column order matches `FromRow` instance

3. **Check `src/Moe/Infra/Database/Episode.hs`**:
   - If episode table was modified, update column lists

4. **Check domain types**:
   - If a new column was added, add the corresponding field to the domain type in `src/Moe/Domain/`
   - Update `FromRow`/`ToRow` instances if needed

### Step 4: Touch Embedded.hs

Migration files are embedded at compile time via `file-embed`. GHC does not track directory listings for TH dependencies, so you must touch the embedding module:

```bash
touch src/Moe/Infra/Database/Embedded.hs
```

### Step 5: Build and Verify

```bash
cabal build
```

Fix any type errors from column mismatches.

## Checklist

- [ ] Migration SQL file created with correct timestamp
- [ ] `bangumiColumnList` updated (if bangumi table changed)
- [ ] All INSERT/UPDATE queries include new columns
- [ ] All SELECT queries include new columns in correct position
- [ ] JOIN queries use `bangumiColumnsAs` (not hardcoded column lists)
- [ ] Domain type updated with new field
- [ ] `FromRow`/`ToRow` instances match column order
- [ ] `Embedded.hs` touched for recompilation
- [ ] `cabal build` succeeds
