# ----------------------------------------------
# Install (if missing) and load required packages
# ----------------------------------------------

required_pkgs <- c(
  "readr",
  "tidyverse",   # includes ggplot2, dplyr, etc.
  "broom",
  "lsr",
  "mice",
  "ggthemes",
  "car",
  "MASS",
  "lubridate",
  "countrycode",
  "DBI",
  "RPostgres",
  "ggfortify",
  "readxl",
  "janitor"
)

# Install any packages that are not yet available
# to_install <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
# if (length(to_install) > 0) {
#   install.packages(to_install, dependencies = TRUE)
# }


library(tidyverse)
library(readxl)
library(readr)
library(countrycode)
library(lsr)
library(janitor)


# importing data cleaned by power query ---------------
  
  # fork sheets from workbook

wb <- "data/ri_sheets.xlsx"

IRENE_grid_capacity <- read_excel(wb, sheet = 1)

IRENE_public_financing <- read_excel(wb, sheet = 2)

wb_dev_indicator <- read_excel(wb, sheet = 3)

  # iso3c code for standardisation


    # (custom iso3 for Kosovo, Channel Island)

custom_iso3 <- c(
  "Kosovo"           = "XKX",   
  "Channel Islands"  = "CHI"    
)

IRENE_grid_iso <- IRENE_grid_capacity %>%
  mutate(country_code = countrycode(country, 
                                    origin = "country.name", 
                                    destination = "iso3c",
                                    custom_match = custom_iso3))

IRENE_financing_iso <- IRENE_public_financing %>%
  mutate(country_code = countrycode(country, 
                                    origin = "country.name", 
                                    destination = "iso3c",
                                    custom_match = custom_iso3))

wb_dev_iso <- wb_dev_indicator %>%
  mutate(country_code = countrycode(country, 
                                    origin = "country.name", 
                                    destination = "iso3c",
                                    custom_match = custom_iso3))




# staging for union -----------------------------------


  IRENE_grid_iso %>%
    distinct(country_code) %>%
    count()
  
  IRENE_financing_iso %>%
    distinct(country_code) %>%
    count()

  wb_dev_iso %>%
    distinct(country_code) %>%
    count()

  
 IRENE_grid_stand.name <- IRENE_grid_iso %>%
    mutate(country = countrycode(country, "country.name", "country.name.en"))
  
IRENE_financing_stand.name <- IRENE_financing_iso %>%
    mutate(country = countrycode(country, "country.name", "country.name.en"))
  
 wb_dev_stand.name <- wb_dev_iso %>%
    mutate(country = countrycode(country, "country.name", "country.name.en"))
 

# union -----------------------------------------------


 combined_table <- bind_rows(
   wb_dev_stand.name,
   IRENE_grid_stand.name,
   IRENE_financing_stand.name
 ) %>%
   select(country_code, country, indicator, starts_with("year_"))
 

# assign continent
continents <- read_csv("data/country-continent-codes.csv")
    
 
ri_master.dataset <- combined_table %>%
  left_join(x=combined_table, 
            y = continents, 
            by = c("country_code" = "iso3"), 
            relationship = "many-to-many") %>%
  select(country_code,
         country = country.x,
         continent,
         indicator:year_2024) %>%
  arrange(country)

# long-to-wide


ri_long <- ri_master.dataset %>% 
  pivot_longer(
    cols = starts_with("year_"),
    names_to = "year",
    names_pattern = "year_(\\d+)",
    values_to = "value",
    values_drop_na = TRUE
  )

# filter + split the indicator string 
ri_clean <- ri_long %>% 
  # remove nuclear & other non-renewables
  filter(!str_detect(indicator, "^(nuclear|other_non_renewable)")) %>% 
  
  # break the indicator into parts
  separate(
    indicator,
    into  = c("tech","metric","unit","grid","misc"),
    fill  = "right",
    extra = "merge"          # glue leftovers into 'misc'
  ) %>% 
  
  # standardise technology names
  mutate(
    tech = case_when(
      tech %in% c("solar_pv", "solar_thermal")              ~ "solar",
      tech %in% c("onshore_wind", "offshore_wind")          ~ "wind",
      tech %in% c("renewable_hydropower", "mixed_hydro")    ~ "hydro",
      TRUE                                                  ~ tech
    ),
    grid = case_when(
      grid %in% c("on_grid",  "on")  ~ "on",
      grid %in% c("off_grid", "off") ~ "off",
      TRUE                            ~ NA_character_
    )
  ) %>% 
  
# aggregate on/off-grid
  
group_by(country_code, country, continent,
         year, tech, metric, unit) %>% 
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
  
# pivot_wider again
  
ri_wide <- ri_clean %>% 
  unite(indicator_clean, tech, metric, unit, sep = "_") %>% 
  pivot_wider(names_from = indicator_clean, values_from = value) %>%
  arrange(country)

# aggregation -------------------------------------------

  
  renewable_inv_cleaned <- ri_wide %>%
    mutate(
      # Aggregated capacities
      total_solar_cap = solar_pv_cap + solar_thermal_cap,
      total_wind_cap  = onshore_wind_cap + offshore_wind_cap,
      total_hydro_cap = renewable_hydropower_cap + mixed_hydro_cap,
      
      # Aggregated generation
      total_solar_gen = solar_pv_gen + solar_thermal_gen,
      total_wind_gen  = onshore_wind_gen + offshore_wind_gen,
      total_hydro_gen = renewable_hydropower_gen + mixed_hydro_gen,
      
      # Aggregated financing
      total_financing_solar   = financing_ongrid_solar + financing_offgrid_solar + financing_solar_thermal + financing_concentrated_solar,
      total_financing_wind    = financing_onshore_wind + financing_offshore_wind,
      total_financing_hydro   = financing_renewable_hydropower + financing_pumped_storage,
      total_financing_other   = financing_marine_energy + financing_nuclear_NA,
      total_financing_renewables = total_financing_solar + total_financing_wind + total_financing_hydro + total_financing_other
    ) %>%
    select(
      country_code, country, continent, year,
      
      # Renewable share and production indicators
      renewable_elec_pct, renewable_prod_kwh, renewable_prod_pct, renewable_consump_pct,
      hydro_elec_pct,
      
      # Emissions and investment
      GHG_emissions_total, energy_invest_ppp, energy_invest_private,
      
      # Aggregated fields
      total_solar_cap, total_solar_gen,
      total_wind_cap, total_wind_gen,
      total_hydro_cap, total_hydro_gen,
      total_renewable_cap, total_renewable_gen,
      total_financing_solar, total_financing_wind,
      total_financing_hydro, total_financing_other,
      total_financing_renewables
    )
  


# SQL export ------------------------------------------

library(DBI)
library(RPostgres)

# Establish connection to PostgreSQL database
con <- dbConnect(
  RPostgres::Postgres(),
  dbname   = "renewable_funding",
  host     = "localhost",
  port     = 5432,
  user     = "postgres",
  password = "123456789"  # Replace with your actual password
)

# Drop the table if it already exists to avoid overwrite conflicts
dbExecute(con, "DROP TABLE IF EXISTS ri_aggregated")

# Write the aggregated R dataset to PostgreSQL
dbWriteTable(
  conn  = con,
  name  = "renewable_inv_cleaned",
  value = renewable_inv_cleaned,
  row.names = FALSE
)

# Confirm successful upload
dbListTables(con)

# Disconnect from the database
dbDisconnect(con)



