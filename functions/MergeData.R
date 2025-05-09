# Load required libraries
library(readr)
library(dplyr)

# Load cleaned energy dataset
energy_transformed <- read_csv("/Users/manasamangipudi/Desktop/Semester-3/DataWrangling/DS597-Group8/data/cleaned/energy_transformed.csv")
print("Energy data loaded successfully!")

# Load cleaned CO2 dataset
co2_transformed <- read_csv("/Users/manasamangipudi/Desktop/Semester-3/DataWrangling/DS597-Group8/data/cleaned/co2_transformed.csv")
print("CO2 data loaded successfully!")

# Remove duplicated columns from CO2 dataset
co2_transformed <- co2_transformed %>%
  select(-iso_code, -population, -gdp, -log_gdp)

# Merge datasets on country and year
energy_co2_merged <- left_join(
  energy_transformed,
  co2_transformed,
  by = c("country", "year")
)

# Create derived variable: CO2 emissions per kWh of electricity
get_co2_electricity_ratio <- function(df) {
  df %>%
    mutate(co2_per_kwh = co2 / electricity_generation)
}

# Apply the transformation
energy_co2_merged <- get_co2_electricity_ratio(energy_co2_merged)

# Save the merged and enriched dataset
write_csv(energy_co2_merged, "/Users/manasamangipudi/Desktop/Semester-3/DataWrangling/Project/data/energy_co2_data_merged.csv")
print("Merged dataset with co2_per_kwh saved successfully!")

