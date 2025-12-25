-- Add is_primary field to RSS table for primary/backup RSS distinction
-- Primary RSS episodes can override episodes downloaded from backup RSS

-- Add is_primary column with default 0 (backup) for backward compatibility
ALTER TABLE rss ADD COLUMN is_primary INTEGER NOT NULL DEFAULT 0;

-- Create unique partial index to ensure only one primary RSS per bangumi
-- This constraint is enforced at database level for data integrity
CREATE UNIQUE INDEX IF NOT EXISTS idx_rss_bangumi_primary
ON rss(bangumi_id) WHERE is_primary = 1;

-- Create index for fast lookups by is_primary flag
CREATE INDEX IF NOT EXISTS idx_rss_is_primary ON rss(is_primary);
