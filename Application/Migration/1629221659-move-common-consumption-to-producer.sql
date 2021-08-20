-- Write your SQL migration code in here
ALTER TABLE co2_producers 
    ADD COLUMN unit units DEFAULT 'hour' NOT NULL,
    ADD COLUMN common_single_consumption_from DOUBLE PRECISION DEFAULT 1.0 NOT NULL,
    ADD COLUMN common_single_consumption_to DOUBLE PRECISION DEFAULT 8.0 NOT NULL,
    ADD COLUMN common_single_consumption_average DOUBLE PRECISION DEFAULT 3.0 NOT NULL,
    ADD COLUMN common_yearly_consumption_from DOUBLE PRECISION DEFAULT 1.0 NOT NULL,
    ADD COLUMN common_yearly_consumption_to DOUBLE PRECISION DEFAULT 8.0 NOT NULL,
    ADD COLUMN common_yearly_consumption_average DOUBLE PRECISION DEFAULT 3.0 NOT NULL;

ALTER TABLE co2_producer_details 
    DROP COLUMN common_consumption,
    DROP COLUMN average_yearly_consumption,
    DROP COLUMN unit;
