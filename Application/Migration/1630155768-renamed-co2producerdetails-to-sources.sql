ALTER TABLE co2_producer_details
    RENAME TO sources;

ALTER TABLE sources
    RENAME COLUMN source TO description;
