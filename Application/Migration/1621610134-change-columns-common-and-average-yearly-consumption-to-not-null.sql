-- Write your SQL migration code in here
ALTER TABLE co2_emitters 
    ALTER COLUMN common_consumption SET NOT NULL,
    ALTER COLUMN average_yearly_consumption SET NOT NULL;
