-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TYPE units AS ENUM ('kilometer', 'hour', 'gram', 'meter', 'kilogram', 'liter', 'minutes');
CREATE TABLE co2_producers (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    description TEXT DEFAULT NULL,
    category_id UUID NOT NULL,
    g_co2e DOUBLE PRECISION NOT NULL,
    per DOUBLE PRECISION NOT NULL,
    unit units NOT NULL,
    source TEXT NOT NULL,
    image TEXT DEFAULT NULL,
    user_id UUID,
    common_consumption DOUBLE PRECISION NOT NULL,
    average_yearly_consumption DOUBLE PRECISION NOT NULL
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
ALTER TABLE co2_producers ADD CONSTRAINT co2_producers_ref_category_id FOREIGN KEY (category_id) REFERENCES categories (id) ON DELETE NO ACTION;
ALTER TABLE co2_producers ADD CONSTRAINT co2_producers_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
