library(targets)
library(tarchetypes)
source("R/Functions.R")
options(tidyverse.quiet = TRUE)

# Set target-specific options such as packages.
tar_option_set(packages = c(
  "tidyverse",
  "ggplot2",
  "patchwork",
  "tidypaleo",
  "here",
  "RColorBrewer",
  "EMMAgeo",
  "kableExtra",
  "dplyr"
  ))

# path_x0 <- here::here("analysis/data/raw_data/","2021-06-16_TT3_data_H01.csv")
# path_tte <- here::here("analysis/data/raw_data/","TTG.csv")
# End this file with a list of target objects.
list(
  tar_target(
    raw_data,
    "analysis/data/raw_data/TTG.csv",
    format = "file"),
  tar_target(
    raw_data2,
    "analysis/data/raw_data/2021-06-16_TT3_data_H01.csv",
    format = "file"),
  tar_target(
    target_data,
    read_write_geodata(raw_data,raw_data2)),
  tar_target(
    raw_geodata,
    "analysis/data/derived_data/datageo.csv",
    format = "file"),
  tar_target(
    strat_plot,
    plot_stratplot(raw_geodata)),
  tar_render(
    paper,
    "analysis/paper/paper.Rmd") #from tarchetype
)



