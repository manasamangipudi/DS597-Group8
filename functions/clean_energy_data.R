library(tidyverse)

load_energy_data <- function(url = "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv") {
  read_csv(url)
}

impute_energy_data <- function(df) {
  df %>%
    group_by(country) %>%
    mutate(across(
      where(is.numeric),
      ~ coalesce(., mean(., na.rm = TRUE)) %>% replace_na(0)
    )) %>%
    ungroup()
}

filter_energy_data <- function(df) {
  df %>%
    filter(!str_starts(iso_code, "OWID"), !is.na(country), !is.na(year)) %>%
    filter(year >= 2000) %>%
    select(where(~ sum(is.na(.)) < 0.4 * nrow(df))) %>%
    filter(!is.na(population), !is.na(gdp))
}

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

add_log_transforms <- function(df) {
  df %>%
    mutate(
      log_gdp = log(gdp),
      log_population = log(population),
      log_electricity = log(electricity_generation+1)
    )
}

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

add_classification_flags <- function(df) {
  df %>%
    mutate(
      high_renewable = if_else(renewables_share_elec > 50, 1, 0),
      transitioning = if_else(renewables_share_elec > fossil_share_elec, 1, 0)
    )
}

transform_energy_data <- function(df) {
  df %>%
    add_normalized_metrics() %>%
    add_log_transforms() %>%
    add_energy_ratios() %>%
    add_classification_flags()
}


na_percentage <- function(df) {
  sapply(df, function(col) round(mean(is.na(col)) * 100, 2))
}
