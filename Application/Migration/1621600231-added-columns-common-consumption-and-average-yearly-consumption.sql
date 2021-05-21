-- Write your SQL migration code in here
ALTER TABLE co2_emitters 
    ADD COLUMN common_consumption DOUBLE PRECISION,
    ADD COLUMN average_yearly_consumption DOUBLE PRECISION;
