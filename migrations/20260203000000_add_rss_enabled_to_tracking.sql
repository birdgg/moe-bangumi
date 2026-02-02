ALTER TABLE tracking ADD COLUMN rss_enabled INTEGER NOT NULL DEFAULT 1;

CREATE TRIGGER tracking_disable_rss_on_collection_insert
AFTER INSERT ON tracking
WHEN NEW.tracking_type = 'collection' AND NEW.rss_enabled = 1
BEGIN
  UPDATE tracking SET rss_enabled = 0 WHERE id = NEW.id;
END;

CREATE TRIGGER tracking_disable_rss_on_collection_update
AFTER UPDATE OF tracking_type ON tracking
WHEN NEW.tracking_type = 'collection' AND NEW.rss_enabled = 1
BEGIN
  UPDATE tracking SET rss_enabled = 0 WHERE id = NEW.id;
END;
