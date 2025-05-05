library(tidyverse)
co2_data <- read_csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv")

filter_co2_data <- function(df) {
  df %>%
    filter(!str_starts(iso_code, "OWID"), !is.na(country), !is.na(year)) %>%
    filter(year >= 2000) %>%
    select(where(~ sum(is.na(.)) < 0.4 * nrow(df))) %>%
    filter(!is.na(population), !is.na(gdp))
}

co2_data = filter_co2_data(co2_data)

co2_data = co2_data %>% select(-c(cement_co2,cement_co2_per_capita,co2_growth_abs,co2_growth_prct,co2_including_luc_growth_abs,co2_including_luc_growth_prct,cumulative_cement_co2,cumulative_co2_including_luc,cumulative_luc_co2,flaring_co2,flaring_co2_per_capita,cumulative_flaring_co2,share_global_flaring_co2,share_global_cumulative_flaring_co2))

#CO₂ emissions year-over-year change
co2_data = co2_data %>%
  group_by(country) %>%
  mutate(co2_pct_change = (co2 - lag(co2)) / lag(co2) * 100)
#Total CO₂ from fossil fuel types 
co2_data = co2_data %>%
  mutate(fossil_fuel_co2 = coal_co2 + oil_co2 + gas_co2)
#log gdp
co2_data = co2_data %>% mutate(log_gdp = log(gdp))
#log co2
co2_data = co2_data %>% mutate(log_co2 = log(co2))
