-- Write your SQL migration code in here
ALTER TABLE co2_emitters
    ALTER COLUMN g_co2e TYPE DOUBLE PRECISION,
    ALTER COLUMN per TYPE DOUBLE PRECISION;