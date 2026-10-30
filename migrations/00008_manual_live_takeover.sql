ALTER TABLE config_live_input ADD COLUMN auto_takeover INTEGER NOT NULL DEFAULT 1
    CHECK (auto_takeover IN (0, 1));
