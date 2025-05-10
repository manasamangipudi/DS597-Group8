library(tidyverse)
library(countrycode)

process_energy_data <- function(raw_data_path, cleaned_data_path) {
  
  # Load Raw Data from source
  load_energy_data <- function(url = "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv") {
    read_csv(url)
  }
  
  # Filter out unnecessary data and high missing values
  filter_data <- function(df) {
    df %>%
      filter(!str_starts(iso_code, "OWID"), !is.na(country), !is.na(year)) %>%
      filter(year >= 2000) %>%
      select(where(~ sum(is.na(.)) < 0.4 * nrow(df))) %>%
      filter(!is.na(population), !is.na(gdp))
  }
  
  # Select only necessary columns
  select_energy_columns <- function(df) {
    df %>%
      select(
        country, iso_code, year, population, gdp,
        electricity_generation,
        renewables_electricity, fossil_electricity,
        solar_electricity, wind_electricity, hydro_electricity,
        renewables_share_elec, coal_share_elec, gas_share_elec, oil_share_elec
      )
  }
  
  # Add normalized metrics
  add_normalized_metrics <- function(df) {
    df %>%
      mutate(
        electricity_per_capita = electricity_generation / population,
        renewables_per_capita = renewables_electricity / population,
        fossil_per_capita = fossil_electricity / population,
        electricity_per_gdp = electricity_generation / gdp,
        gdp_per_electricity = gdp / (electricity_generation + 1)
      )
  }
  
  # Log transforms
  add_log_transforms <- function(df) {
    df %>%
      mutate(
        log_gdp = log(gdp + 1),
        log_population = log(population + 1),
        log_electricity = log(electricity_generation + 1)
      )
  }
  
  # Add energy ratios
  add_energy_ratios <- function(df) {
    df %>%
      mutate(
        fossil_to_renewable_ratio = fossil_electricity / (renewables_electricity + 1),
        fossil_share_elec = fossil_electricity / (electricity_generation + 1),
        solar_share = solar_electricity / (renewables_electricity + 1),
        wind_share = wind_electricity / (renewables_electricity + 1),
        hydro_share = hydro_electricity / (renewables_electricity + 1)
      )
  }
  
  # Add classification flags
  add_classification_flags <- function(df) {
    df %>%
      mutate(
        high_renewable = if_else(renewables_share_elec > 50, 1, 0),
        transitioning = if_else(renewables_share_elec > fossil_share_elec, 1, 0)
      )
  }
  
  # Combine transformations
  transform_energy_data <- function(df) {
    df %>%
      add_normalized_metrics() %>%
      add_log_transforms() %>%
      add_energy_ratios() %>%
      add_classification_flags()
  }
  
  # Check missing percentage
  na_percentage <- function(df) {
    sapply(df, function(col) round(mean(is.na(col)) * 100, 2))
  }
  
  # ---- RUNNING THE FULL WORKFLOW ----
  
  energy_raw <- load_energy_data()
  cat("Number of rows in Raw Dataset:", nrow(energy_raw), '\n')
  cat("Number of columns in Raw Dataset:", ncol(energy_raw), '\n')
  write_csv(energy_raw, raw_data_path)
  cat("Raw dataset saved to", raw_data_path, '\n')
  
  energy_clean <- filter_data(energy_raw)
  cat("Number of rows after filtering:", nrow(energy_clean), '\n')
  cat("Number of columns after filtering:", ncol(energy_clean), '\n')
  
  energy_selected <- select_energy_columns(energy_clean)
  cat("Columns after selecting important ones:", ncol(energy_selected), '\n')
  
  energy_transformed <- transform_energy_data(energy_selected)
  
  # Add continent info
  energy_transformed <- energy_transformed %>%
    mutate(continent = countrycode(country, "country.name", "continent"))
  
  cat("Final dataset columns after transformations:", ncol(energy_transformed), '\n')
  
  # Duplicate and missing check
  duplicated_rows <- energy_transformed %>% filter(duplicated(.))
  if (nrow(duplicated_rows) > 0) {
    cat("Warning: Duplicated rows detected:", nrow(duplicated_rows), '\n')
  }
  
  cat("Percentage of missing values in each column:\n")
  print(na_percentage(energy_transformed))
  
  # Save the result
  write_csv(energy_transformed, cleaned_data_path)
  cat("Final transformed dataset saved to", cleaned_data_path, '\n')
  
  
  return(energy_transformed)
}
