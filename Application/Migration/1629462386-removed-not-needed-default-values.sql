-- Write your SQL migration code in here
ALTER TABLE co2_producers 
  ALTER COLUMN user_id SET NOT NULL,
  ALTER COLUMN common_single_consumption_from SET DEFAULT NULL,
  ALTER COLUMN common_single_consumption_to SET DEFAULT NULL,
  ALTER COLUMN common_single_consumption_average SET DEFAULT NULL,
  ALTER COLUMN common_yearly_consumption_from SET DEFAULT NULL,
  ALTER COLUMN common_yearly_consumption_to SET DEFAULT NULL,
  ALTER COLUMN common_yearly_consumption_average SET DEFAULT NULL,
  ALTER COLUMN unit SET DEFAULT NULL;

ALTER TABLE co2_producer_details ALTER COLUMN user_id SET NOT NULL;