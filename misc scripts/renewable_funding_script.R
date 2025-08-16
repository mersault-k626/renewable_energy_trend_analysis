# =============================================================================

# R Script Documentation ------------------------------

# =============================================================================

# 1. INTRODUCTION ============================================================

# This report analyses global and regional trends in renewable energy investment 
# and innovation, focusing on the distribution of funding across countries and 
# sectors (solar, wind, hydro, etc.) from 2015 to 2024. The objective is to 
# identify which countries and technologies are leading the transition to clean 
# energy, how investments have evolved over time, and where future opportunities 
# or gaps exist. The findings are based on the latest open-source datasets and 
# aim to provide actionable insights for policymakers, investors, and stakeholders 
# in the energy transition.

# 2. DATA SOURCES AND METHODOLOGY ============================================

# Data Sources:
# - International Energy Agency (IEA) — World Energy Investment 2025 Datafile: 
#   Annual data on energy investment trends by region and technology.
# - World Bank — World Development Indicators: National indicators for energy 
#   investment, clean energy policy, and development outcomes.
# - International Renewable Energy Agency (IRENA) — IRENASTAT: Country-level 
#   statistics on renewable energy capacity, generation, and technology breakdowns.
# - United Nations Data Portal — UN Data Explorer: Supplementary country and 
#   economic indicators.

# Data Extraction Tools Used:
# - Microsoft Excel (with Power Query): Initial filtering, sheet extraction, 
#   and tabular conversion.
# - R packages: tidyverse, readxl, countrycode, janitor for cleaning, 
#   transformation, unification of variable names, and aggregation.

# 3. DATA CLEANING & PREPARATION ==============================================

# Load required libraries
library(tidyverse)
library(readxl)
library(countrycode)
library(janitor)

# Process Overview:

# Data Import and Integration
# Data was imported from Excel and CSV files, including country-level, regional, 
# and sectoral tables. Three core data tables (ire_grid, ire_fin, wb_dev) were 
# standardised and combined using consistent country codes (ISO3C), with special 
# handling for unmatched names (e.g., Kosovo, Channel Islands) using tidyverse.

# Variable Harmonisation
# - Country names and codes were cleaned using the countrycode package for 
#   global comparability.
# - Columns were renamed for clarity and grouped by key categories (technology, 
#   metric, unit, grid, etc.).

# Pivoting and Aggregation:
# - Data was pivoted to a tidy format, separating years and indicators for 
#   easier analysis.
# - Sector names were unified (e.g., combining "solar_pv" and "solar_thermal" 
#   as "solar").
# - Technology metrics (capacity, generation, investment) were aggregated by 
#   country, region, year, and sector.

# Missing Values and Outliers:
# - Missing and non-numeric values were handled by excluding or aggregating 
#   only valid data points (using na.rm = TRUE).
# - Nuclear and non-renewable indicators were filtered out to focus exclusively 
#   on clean energy.

# Final Output:
# The process yielded two primary cleaned tibbles:
# - wei_region_investment: Regional/yearly investment totals by sector and 
#   technology (in billion USD).
# - renewable_inv_cleaned: Country-level panel data (2015–2024) with capacity, 
#   generation, financing, and investment breakdowns for solar, wind, hydro, 
#   and aggregate renewables.

# =============================================================================
# END OF DOCUMENTATION
# =============================================================================#

#################################################################################################

# --- Prerequisite packages ---------------------------

  # install if missing

required_pkgs <- c("tidyverse",
                   "readxl",
                   "countrycode",
                   "janitor",
                   "DBI",
                   "RPostgres",
                   "lubridate",
                   "here"
                   )


installed <- installed.packages()[, "Package"]

to_install <- setdiff(required_pkgs, installed)

if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

invisible(lapply(required_pkgs, library, character.only = TRUE))



# --- Define path to data -----------------------------

wb_url <- "https://github.com/mersault-k626/renewable_energy_trend_analysis/raw/refs/heads/main/data/raw/ri_sheets.xlsx"

wb_temp <- tempfile(fileext = ".xlsx")
download.file(wb_url, wb_temp, mode = "wb")

continents_path <- "https://raw.githubusercontent.com/mersault-k626/renewable_energy_trend_analysis/refs/heads/main/data/raw/country-continent-codes.csv"

# ---- Read raw Excel sheets ----

ire_grid <- read_excel(wb_temp, sheet = 1)
ire_fin  <- read_excel(wb_temp, sheet = 2)
wb_dev   <- read_excel(wb_temp, sheet = 3)


# --- New columns for ISO3C with custom matches -------


custom_iso3 <- c(Kosovo = "XKX", `Channel Islands` = "CHI")


add_iso3 <- function(df) { #custom countrycode function
  df %>%
    mutate(country_code = countrycode(country,
                                      origin = "country.name",
                                      destination  = "iso3c",
                                      custom_match = custom_iso3
                                      )
    )
  }


ire_grid_iso <- add_iso3(ire_grid) %>%
  mutate(country = countrycode(country,
                               "country.name",
                               "country.name.en"))


ire_fin_iso  <- add_iso3(ire_fin) %>%
  mutate(country = countrycode(country, "country.name", "country.name.en"))


wb_dev_iso   <- add_iso3(wb_dev) %>%
  mutate(country = countrycode(country, "country.name", "country.name.en"))




# --- Combine relevant columns ------------------------

combined <- bind_rows(wb_dev_iso, ire_grid_iso, ire_fin_iso) %>%
  select(country_code, country, indicator, starts_with("year_"))




# --- Load continents files, assign column ------------


continents <- read_csv(continents_path) %>%
  clean_names() %>%
  select(continent, iso3)


ri_master <- combined %>%
  left_join(continents, by = c("country_code" = "iso3"), relationship = "many-to-many") %>%
  select(
    country_code,
    country,   # rename it back
    continent,
    indicator,
    starts_with("year_")
  )




# --- Pivot wider -------------------------------------


ri_wide <- ri_master %>%
  pivot_longer(cols = starts_with("year_"),
               names_to = "year", 
               names_prefix   = "year_", 
               values_to      = "value",
               values_drop_na = TRUE
  ) %>%
  
  filter(!str_detect(indicator, "^(nuclear|other_non_renewable)")) %>%
  
  separate(indicator,
           into = c("tech", "metric", "unit", "grid", "misc"),
           fill = "right",
           extra = "merge"
           ) %>%
  
  mutate(tech = case_when(tech %in% c("solar_pv", "solar_thermal") ~ "solar",
                          tech %in% c("onshore_wind", "offshore_wind") ~ "wind",
                          tech %in% c("renewable_hydropower", "mixed_hydro") ~ "hydro",
                          TRUE ~ tech
                          ),
    grid = case_when(grid %in% c("on_grid", "on") ~ "on",
                     grid %in% c("off_grid", "off") ~ "off",
                     TRUE ~ NA_character_
                     )
    ) %>%
  
  group_by(country_code, 
           country, 
           continent, 
           year, 
           tech, 
           metric, 
           unit) %>%
  
  summarise(value = sum(value, na.rm = TRUE), 
            .groups = "drop") %>%
  
  unite("indicator_clean", 
        tech, 
        metric, 
        unit, 
        sep = "_") %>%
  
  pivot_wider(names_from = indicator_clean, 
              values_from = value) %>%
  arrange(country_code)




# ---Specify unit (USD, mw, kwh, gwh) -----------------


ri_wide <- ri_wide %>%
  rename_with(~ paste0(.x, "_USD"),
              .cols = starts_with("energy_invest_") | starts_with("financing_")) %>%
  rename_with(~ paste0(.x, "_mw"),
              .cols = ends_with("_cap") & !ends_with("_mw")
              ) %>%
  rename_with(~ paste0(.x, "_gwh"),
              .cols = ends_with("_gen") & !ends_with("_gwh")
              )



# ---Aggregate data -----------------------------------


renewable_inv_cleaned <- ri_wide %>%
  mutate(
    # Aggregated capacities (MW)
    total_solar_cap_mw = rowSums(across(c(solar_pv_cap_mw, solar_thermal_cap_mw)), na.rm = TRUE),
    total_wind_cap_mw  = rowSums(across(c(onshore_wind_cap_mw, offshore_wind_cap_mw)), na.rm = TRUE),
    total_hydro_cap_mw = rowSums(across(c(renewable_hydropower_cap_mw, mixed_hydro_cap_mw)), na.rm = TRUE),
    
    # Aggregated generation (GWh)
    total_solar_gen_gwh = rowSums(across(c(solar_pv_gen_gwh, solar_thermal_gen_gwh)), na.rm = TRUE),
    total_wind_gen_gwh  = rowSums(across(c(onshore_wind_gen_gwh, offshore_wind_gen_gwh)), na.rm = TRUE),
    total_hydro_gen_gwh = rowSums(across(c(renewable_hydropower_gen_gwh, mixed_hydro_gen_gwh)), na.rm = TRUE),
    
    # Aggregated financing (USD)
    total_financing_solar_USD      = rowSums(across(c(financing_ongrid_solar_USD,
                                                      financing_offgrid_solar_USD,
                                                      financing_solar_thermal_USD,
                                                      financing_concentrated_solar_USD)),
                                             na.rm = TRUE),
    total_financing_wind_USD       = rowSums(across(c(financing_onshore_wind_USD,
                                                      financing_offshore_wind_USD)),
                                             na.rm = TRUE),
    total_financing_hydro_USD      = rowSums(across(c(financing_renewable_hydropower_USD,
                                                      financing_pumped_storage_USD)),
                                             na.rm = TRUE),
    total_financing_other_USD      = rowSums(across(c(financing_marine_energy_USD,
                                                      financing_nuclear_NA_USD)),
                                             na.rm = TRUE),
    total_financing_renewables_USD = rowSums(across(starts_with("total_financing_")), na.rm = TRUE),
    
    # Combine private & PPP investment into one column
    energy_invest_private_ppp_USD = case_when(
      !is.na(energy_invest_private_USD) &
        !is.na(energy_invest_ppp_USD) &
        energy_invest_private_USD == energy_invest_ppp_USD ~ energy_invest_private_USD,
      
      !is.na(energy_invest_private_USD) &
        !is.na(energy_invest_ppp_USD) ~ energy_invest_private_USD + energy_invest_ppp_USD,
      
      TRUE ~ coalesce(energy_invest_private_USD, energy_invest_ppp_USD)
    )
  ) %>%
  select(
    country_code,
    country,
    continent,
    year,
    
    # % shares & totals
    renewable_elec_pct,
    renewable_prod_kwh,
    renewable_prod_pct,
    renewable_consump_pct,
    hydro_elec_pct,
    total_renewable_cap_mw,
    total_renewable_gen_gwh,
    
    # emissions & investment (USD)
    GHG_emissions_total,
    energy_invest_private_ppp_USD,
    
    # by-tech aggregates
    total_solar_cap_mw,
    total_solar_gen_gwh,
    total_wind_cap_mw,
    total_wind_gen_gwh,
    total_hydro_cap_mw,
    total_hydro_gen_gwh,
    
    # financing aggregates (USD)
    total_financing_solar_USD,
    total_financing_wind_USD,
    total_financing_hydro_USD,
    total_financing_other_USD,
    total_financing_renewables_USD
  )

write_csv(renewable_inv_cleaned, file = "data/renewable_inv_cleaned.csv")


# ---SQL export ---------------------------------------


# parameters (via env vars or defaults)

host   <- Sys.getenv("PG_HOST", "localhost")

port   <- Sys.getenv("PG_PORT", "5432")

user   <- Sys.getenv("PG_USER", "postgres")

pwd    <- Sys.getenv("PG_PWD", "123456789")

dbname <- Sys.getenv("PG_DB", "renewable_funding")

# Establish connection to PostgreSQL

default_con <- dbConnect(Postgres(),
                         host = host,
                         port = port,
                         dbname = "postgres",
                         user = user,
                         password = pwd
                         )

# Check if db exist; if not create one

exists <- dbGetQuery(default_con, 
                     sprintf("SELECT 1 FROM pg_database WHERE datname = '%s'", dbname)
                     )
if (nrow(exists) == 0) {
  dbExecute(default_con, sprintf("CREATE DATABASE %s", dbname))
}

dbDisconnect(default_con)

# Connect to db and export cleaned dataset

con <- dbConnect(Postgres(),
                 host = host,
                 port = port,
                 dbname = dbname,
                 user = user,
                 password = pwd
)

if (dbExistsTable(con, "renewable_investment")) {
  dbExecute(con, "DROP TABLE renewable_investment")
}

dbWriteTable(conn = con,
             name = "renewable_investment",
             value = renewable_inv_cleaned,
             overwrite = TRUE,
             row.names = FALSE
             )

dbDisconnect(con)

# proceed to SQL for normalisation


renewable_inv_cleaned %>%
  filter(country_code == "ABW")


# ---WEI ----------------------------------------------

wei_path <- "https://github.com/mersault-k626/renewable_energy_trend_analysis/raw/refs/heads/main/data/wei.xlsx"



wei_temp <- tempfile(fileext = ".xlsx")
download.file(wei_path, wei_temp, mode = "wb")

sheet_names <- excel_sheets(wei_temp)

for (sheet in sheet_names) {
  var_name <- make.names(sheet)
  assign(var_name, read_excel(wei_temp, sheet = sheet))
}

wei_region_investment <- rbind(africa, asia_pacific, china, eurasia, europe, middle_east, north_america, world)


# analysis --------------------------------------------

region_labels <- c(
  africa = "Africa",
  asia_pacific = "Asia-Pacific",
  china = "China",
  eurasia = "Eurasia",
  europe = "Europe",
  middle_east = "Middle East",
  north_america = "North America",
  world = "Global"
)


ggplot(wei_region_investment, aes(x = as.numeric(year), y = total_clean_energy_usd_billion_2024_mer, colour = location)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 2015:2025) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA)) +
  labs(
    x = "Year",
    y = "Clean Energy Investment (Billion USD, 2024 MER)",
    colour = "Region"
  ) +
  facet_wrap(~location, scales = "free_y", labeller = as_labeller(region_labels)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line(),
    legend.position = "none"
  )




