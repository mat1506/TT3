library(targets)
library(tarchetypes)
library(tidyverse)
library(here)
source("R/Functions.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "ggplot2", "patchwork",
                            "tidypaleo", "here"))

tt <- read_csv(here::here("analysis/data/raw_data/2021-06-16_TT3_data_H01.csv"))

# End this file with a list of target objects.
list(
  tar_target(
    proc_data,
    data_setup(tt))
)



