-- Write your SQL migration code in here
ALTER TABLE co2_emitters 
    ADD COLUMN main_co2_producer_detail_id UUID DEFAULT NULL,
    DROP COLUMN g_co2e,
    DROP COLUMN common_consumption,
    DROP COLUMN average_yearly_consumption,
    DROP COLUMN per,
    DROP COLUMN unit,
    DROP COLUMN source;