-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE units AS ENUM ('kilometer', 'hour', 'gram', 'meter', 'kilogram', 'liter', 'minutes');
CREATE TABLE co2_producers (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    description TEXT DEFAULT NULL,
    category_id UUID NOT NULL,
    unit units DEFAULT 'hour' NOT NULL,
    common_single_consumption_from DOUBLE PRECISION DEFAULT 1.0 NOT NULL,
    common_single_consumption_to DOUBLE PRECISION DEFAULT 8.0 NOT NULL,
    common_single_consumption_average DOUBLE PRECISION DEFAULT 3.0 NOT NULL,
    common_yearly_consumption_from DOUBLE PRECISION DEFAULT 1.0 NOT NULL,
    common_yearly_consumption_to DOUBLE PRECISION DEFAULT 8.0 NOT NULL,
    common_yearly_consumption_average DOUBLE PRECISION DEFAULT 3.0 NOT NULL,
    image TEXT DEFAULT NULL,
    user_id UUID
);
CREATE TABLE categories (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL
);
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE TABLE co2_producer_details (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    co2_producer_id UUID NOT NULL,
    region TEXT DEFAULT NULL,
    "year" INT DEFAULT NULL,
    g_co2e DOUBLE PRECISION NOT NULL,
    per DOUBLE PRECISION NOT NULL,
    source TEXT NOT NULL,
    user_id UUID DEFAULT NULL
);
CREATE INDEX co2_producer_details_co2_producer_id_index ON co2_producer_details (co2_producer_id);
CREATE INDEX co2_producer_details_user_id_index ON co2_producer_details (user_id);
ALTER TABLE co2_producer_details ADD CONSTRAINT co2_producer_details_ref_co2_producer_id FOREIGN KEY (co2_producer_id) REFERENCES co2_producers (id) ON DELETE NO ACTION;
ALTER TABLE co2_producer_details ADD CONSTRAINT co2_producer_details_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
ALTER TABLE co2_producers ADD CONSTRAINT co2_producers_ref_category_id FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE NO ACTION;
ALTER TABLE co2_producers ADD CONSTRAINT co2_producers_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
