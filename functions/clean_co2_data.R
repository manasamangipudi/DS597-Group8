library(tidyverse)

process_co2_data <- function(raw_data_path, cleaned_data_path) {
  
  # Load raw CO2 data
  url = "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
  co2_data <- read_csv(url)
  # Save the result
  write_csv(co2_data, raw_data_path)
  cat("Raw dataset saved to", raw_data_path, '\n')
  
  # Filter out OWID aggregates, missing values, and keep years from 2000 onwards
  co2_data <- co2_data %>%
    filter(!str_starts(iso_code, "OWID"), !is.na(country), !is.na(year)) %>%
    filter(year >= 2000) %>%
    select(where(~ sum(is.na(.)) < 0.4 * nrow(co2_data))) %>%
    filter(!is.na(population), !is.na(gdp))
  
  # Drop selected unused or redundant columns
  co2_data <- co2_data %>%
    select(
      -c(cement_co2, cement_co2_per_capita,
         co2_growth_abs, co2_growth_prct,
         co2_including_luc_growth_abs, co2_including_luc_growth_prct,
         cumulative_cement_co2, cumulative_co2_including_luc, cumulative_luc_co2,
         flaring_co2, flaring_co2_per_capita,
         cumulative_flaring_co2, share_global_flaring_co2,
         share_global_cumulative_flaring_co2)
    )
  
  # Add % change in CO2 year over year
  co2_data <- co2_data %>%
    group_by(country) %>%
    mutate(co2_pct_change = (co2 - lag(co2)) / lag(co2) * 100) %>%
    ungroup()
  
  # Add total fossil fuel CO2
  co2_data <- co2_data %>%
    mutate(fossil_fuel_co2 = coal_co2 + oil_co2 + gas_co2)
  
  co2_data <- co2_data %>%
    mutate(
      log_gdp = log(gdp + 1),
      log_co2 = log(co2 + 1)
    )
  
  write_csv(co2_data, cleaned_data_path)
  cat("Final transformed dataset saved to", cleaned_data_path, '\n')
  
  return(co2_data)
}
