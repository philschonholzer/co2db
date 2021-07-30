-- Write your SQL migration code in here
CREATE TABLE co2_producer_details (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    co2_producer_id UUID NOT NULL,
    region TEXT DEFAULT NULL,
    "year" INT DEFAULT NULL,
    g_co2e DOUBLE PRECISION NOT NULL,
    common_consumption DOUBLE PRECISION NOT NULL,
    average_yearly_consumption DOUBLE PRECISION NOT NULL,
    per DOUBLE PRECISION NOT NULL,
    unit units NOT NULL,
    source TEXT NOT NULL,
    user_id UUID DEFAULT NULL
);
