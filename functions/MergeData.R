library(readr)
library(dplyr)


merge_energy_co2 <- function(energy_transformed, co2_transformed, output_path ) {
  
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
  energy_co2_merged <- energy_co2_merged %>%
    mutate(co2_per_kwh = co2 / electricity_generation)
 
    write_csv(energy_co2_merged, output_path)
    message("Merged dataset saved to: ", output_path)
  
  return(energy_co2_merged)
}
