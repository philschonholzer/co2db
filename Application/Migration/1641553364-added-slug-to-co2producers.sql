ALTER TABLE co2_producers ADD COLUMN slug TEXT DEFAULT null;
ALTER TABLE co2_producers ADD CONSTRAINT co2_producers_slug_key UNIQUE(slug);
