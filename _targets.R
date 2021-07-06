library(targets)
library(tarchetypes)
library(tidyverse)
library(here)
library(EMMAgeo)
source("R/Functions.R")

# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "ggplot2", "patchwork",
                            "tidypaleo", "here","RColorBrewer"))

tt <- read_csv(here::here("analysis/data/raw_data/2021-06-16_TT3_data_H01.csv"))
ttg <- read_csv(here::here("analysis/data/raw_data/TTG.csv"),col_names=T) %>%
  mutate_at(vars(ID), factor) %>%
  column_to_rownames(var = "ID")
# End this file with a list of target objects.
list(
  tar_target(
    proc_data,
    X_setup(ttg))
)



