
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

# Add normalized metrics : based on population and GDP
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

# Log Transforms - Highly skewed variables like population, electricity generation 
add_log_transforms <- function(df) {
  df %>%
    mutate(
      log_gdp = log(gdp + 1),
      log_population = log(population + 1),
      log_electricity = log(electricity_generation + 1)
    )
}

# Check missing percentage
na_percentage <- function(df) {
  sapply(df, function(col) round(mean(is.na(col)) * 100, 2))
}

# Add energy ratios - gives a perspective of percentages
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



# Load and process
energy_raw <- load_energy_data()
#write_csv(energy_raw, "energy_raw.csv")
cat("Number of rows in Raw Dataset :",  nrow(energy_raw),'\n')
cat("Number of columns in Raw Dataset :", ncol(energy_raw),'\n')

energy_clean <- filter_data(energy_raw)
cat("Number of rows in Filtered Dataset :",  nrow(energy_clean),'\n')
cat("Number of columns in Filtered Dataset :", ncol(energy_clean),'\n')

energy_selected <- select_energy_columns(energy_clean)

cat("Number of columns in Dataset after dropping unnecessary columns:", ncol(energy_selected),'\n')

energy_transformed <- transform_energy_data(energy_selected)

# Add continent info
energy_transformed <- energy_transformed %>%
  mutate(continent = countrycode(country, "country.name", "continent"))

cat("Number of columns in Dataset after creating new columns and transformations:", ncol(energy_transformed),'\n')

### Checking for duplicated and % of missing values
energy_transformed %>% filter(duplicated(.))

### Checking for missing values
cat("Percentage of Missing Values left :", na_percentage(energy_transformed))

write_csv(energy_transformed, "energy_transformed.csv")