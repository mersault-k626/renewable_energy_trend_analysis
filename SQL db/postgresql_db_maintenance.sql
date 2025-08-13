-- Add primary key constraint
ALTER TABLE renewable_investment 
ADD CONSTRAINT pk_renewable_investment 
PRIMARY KEY (country_code, year);

-- Add NOT NULL constraints for essential fields
ALTER TABLE renewable_investment 
ALTER COLUMN country_code SET NOT NULL;

ALTER TABLE renewable_investment 
ALTER COLUMN country SET NOT NULL;

ALTER TABLE renewable_investment 
ALTER COLUMN year SET NOT NULL;

-- Create indexes for better query performance
CREATE INDEX idx_renewable_country ON renewable_investment(country);
CREATE INDEX idx_renewable_continent ON renewable_investment(continent);
CREATE INDEX idx_renewable_year ON renewable_investment(year);
CREATE INDEX idx_renewable_elec_pct ON renewable_investment(renewable_elec_pct);


COMMENT ON TABLE renewable_investment IS 'Renewable energy investment and production data by country and year';

select
	count(*)
from renewable_investment ri 