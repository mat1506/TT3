# Load required packages

library(tidyverse)
# install.packages("remotes")
#remotes::install_github("paleolimbot/tidypaleo")
library(tidypaleo)
library(ggplot2)
library(patchwork)

# Read in the geological data from a CSV file and store it in a variable called geodata

path <- here::here("analysis/data/derived_data/","datageo.csv")
geodata <- read_csv(path, col_types =cols())

# Define zone_data as a tibble with ymin, ymax, xmin, and xmax columns,
# which will be used to create a shaded rectangle in the first plot

zone_data <- tibble(ymin = c(38,140,240),
                    ymax = c(50,170,268),
                    xmin = -Inf, xmax = Inf)  ###### Stratiplot

# Select a subset of columns from the geodata data frame, rename them,
# convert the data frame from wide to long format, filter out any irrelevant values,
# and reorder the remaining values

geodata_longer <- geodata %>%
  dplyr::select(depth,clay,silt_fine,silt_coarse,sand,
                TOC,TIC,TC,TN,
                d13C, d18O) %>%
  rename(Depth = depth,`Clay`=clay,
         `Silt fine`=silt_fine,
         `Silt coarse`=silt_coarse,
         `Sand`=sand,
         `d13C`=d13C,
         `d18O`=d18O) %>%
  pivot_longer(cols =Clay:d18O,names_to = "values", values_to = "count") %>%
  filter(values %in% c("Sand", "Silt fine", "Silt coarse","Clay",
                      "TOC","TIC","TC","TN","d13C", "d18O")) %>%
  mutate(values = fct_relevel(values,"Sand", "Silt fine","Silt coarse",
                             "Clay","TC","TIC","TOC","TN","d13C", "d18O"))

# Create the first plot using ggplot,
# which shows the distribution of various geological variables with depth

alta_plot <- ggplot(geodata_longer,aes(x = count, y = Depth)) +
  geom_lineh() +
  geom_point() +
  geom_rect(mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
            data = zone_data,
            alpha = 0.4,
            fill = "blue",inherit.aes = FALSE ) +
  scale_y_reverse(breaks = c(40,60,80,100,120,140,
                    160,180,200,220,240,260,280)) +
  facet_geochem_gridh(
    vars(values),
    units = c("d13C" = "", "d18O" = ""),
    default_units = "%") +
  labs(x = NULL, y = "Depth (cm)") +
  scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
  theme_set(theme_paleo(8)) +
  theme(
    text = element_text(size=12),
    axis.ticks = element_line(colour = "grey70", size = 0.2),
    panel.grid.major =element_blank(),
    panel.grid.minor = element_blank()
            )

# Use the coniss package to create a cluster plot of the same geological variables

coniss <- geodata_longer %>% ###### Cluster plot
    nested_data(qualifiers = Depth, key = values, value = count, trans = scale) %>%
    nested_chclust_coniss()

# Combine the two plots into a single figure using the wrap_plots function from the cowplot package

Fig2 <- wrap_plots(
  alta_plot +
    theme(strip.background = element_blank(), strip.text.y = element_blank()),
  ggplot() +
    layer_zone_boundaries(coniss, aes(y = Depth)) +
    layer_dendrogram(coniss, aes(y = Depth), param = "CONISS") +
    scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
    theme(axis.text.y.left = element_blank(),
          axis.ticks.y.left = element_blank(),
          text = element_text(size=8))+
    labs(x = "coniss", y = NULL),
  nrow = 1,
  widths = c(9, 0.8)
)

pdf_out <- here::here("analysis/figures","Fig2.pdf")
png_out <- here::here("analysis/figures","Fig2.png")

ggsave(pdf_out, width = 24,height = 12, units = 'cm',device = cairo_pdf)
ggsave(png_out,  width = 24,height = 12, units = 'cm',device = "png")

