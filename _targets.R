library(targets)

tar_option_set(packages = c("tidyverse", "readr", "dplyr"))

source("functions/clean_energy_data.R")
source("functions/clean_co2_data.R")
source("functions/MergeData.R")

list(
  tar_target(energy_data, process_energy_data("data/raw/energy_raw.csv","data/cleaned/energy_transformed.csv")),
  tar_target(co2_data, process_co2_data("data/raw/co2_raw.csv","data/cleaned/co2_transformed.csv")),
  tar_target(merged_data, merge_energy_co2(energy_data, co2_data, "data/merged_data.csv"))
)
