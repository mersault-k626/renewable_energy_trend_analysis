library(DBI)
library(dplyr)

con <- dbConnect(Postgres(),
                 host   = host,
                 port   = port,
                 dbname = dbname,
                 user   = user,
                 password = pwd)

sql_stats <- dbGetQuery(con, '
SELECT
  COUNT(*)                                  AS row_count,
  SUM(renewable_elec_pct)                   AS sum_renewable_elec_pct,
  SUM(renewable_prod_kwh)                   AS sum_renewable_prod_kwh,
  SUM(renewable_prod_pct)                   AS sum_renewable_prod_pct,
  SUM(renewable_consump_pct)                AS sum_renewable_consump_pct,
  SUM(hydro_elec_pct)                       AS sum_hydro_elec_pct,
  SUM(total_renewable_cap_mw)               AS sum_total_renewable_cap_mw,
  SUM(total_renewable_gen_gwh)              AS sum_total_renewable_gen_gwh,
  COUNT("energy_invest_private_ppp_USD")    AS non_null_energy_invest,
  SUM("energy_invest_private_ppp_USD")      AS sum_energy_invest,
  SUM(total_solar_cap_mw)                   AS sum_total_solar_cap_mw,
  SUM(total_solar_gen_gwh)                  AS sum_total_solar_gen_gwh,
  SUM(total_wind_cap_mw)                    AS sum_total_wind_cap_mw,
  SUM(total_wind_gen_gwh)                   AS sum_total_wind_gen_gwh,
  SUM(total_hydro_cap_mw)                   AS sum_total_hydro_cap_mw,
  SUM(total_hydro_gen_gwh)                  AS sum_total_hydro_gen_gwh,
  SUM("total_financing_solar_USD")          AS sum_fin_solar,
  SUM("total_financing_wind_USD")           AS sum_fin_wind,
  SUM("total_financing_hydro_USD")          AS sum_fin_hydro,
  SUM("total_financing_other_USD")          AS sum_fin_other,
  SUM("total_financing_renewables_USD")     AS sum_fin_total
FROM renewable_investment
')

r_stats <- renewable_inv_cleaned %>%
  summarise(
    row_count                    = n(),
    sum_renewable_elec_pct       = sum(renewable_elec_pct, na.rm = TRUE),
    sum_renewable_prod_kwh       = sum(renewable_prod_kwh, na.rm = TRUE),
    sum_renewable_prod_pct       = sum(renewable_prod_pct, na.rm = TRUE),
    sum_renewable_consump_pct    = sum(renewable_consump_pct, na.rm = TRUE),
    sum_hydro_elec_pct           = sum(hydro_elec_pct, na.rm = TRUE),
    sum_total_renewable_cap_mw   = sum(total_renewable_cap_mw, na.rm = TRUE),
    sum_total_renewable_gen_gwh  = sum(total_renewable_gen_gwh, na.rm = TRUE),
    non_null_energy_invest       = sum(!is.na(energy_invest_private_ppp_USD)),
    sum_energy_invest            = sum(energy_invest_private_ppp_USD, na.rm = TRUE),
    sum_total_solar_cap_mw       = sum(total_solar_cap_mw, na.rm = TRUE),
    sum_total_solar_gen_gwh      = sum(total_solar_gen_gwh, na.rm = TRUE),
    sum_total_wind_cap_mw        = sum(total_wind_cap_mw, na.rm = TRUE),
    sum_total_wind_gen_gwh       = sum(total_wind_gen_gwh, na.rm = TRUE),
    sum_total_hydro_cap_mw       = sum(total_hydro_cap_mw, na.rm = TRUE),
    sum_total_hydro_gen_gwh      = sum(total_hydro_gen_gwh, na.rm = TRUE),
    sum_fin_solar                = sum(total_financing_solar_USD, na.rm = TRUE),
    sum_fin_wind                 = sum(total_financing_wind_USD, na.rm = TRUE),
    sum_fin_hydro                = sum(total_financing_hydro_USD, na.rm = TRUE),
    sum_fin_other                = sum(total_financing_other_USD, na.rm = TRUE),
    sum_fin_total                = sum(total_financing_renewables_USD, na.rm = TRUE)
  )

sql_stats <- as.data.frame(sql_stats)
r_stats   <- as.data.frame(r_stats)
all.equal(sql_stats, r_stats)

dbDisconnect(con)
