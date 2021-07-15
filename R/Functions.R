
# 1. Setup create geodata function --------------------------------------------------


read_write_geodata <- function(path_x0,path_tte) {

# Load data ---------------------------------------------------------------

  X <- read_csv(path_x0, col_types =cols(),col_names=T) %>%
    mutate_at(vars(depth), factor) %>%
    column_to_rownames(var = "depth") %>%
    as.matrix()

  X0 <- read_csv(path_tte, col_types =cols())

# Krumbein phi scale ---------------------------------------------------------

  phi <- as.numeric(colnames(X))

# get l and q -------------------------------------------------------------------

  l <- get.l(X = X, max = 0.95, n = 20)
  q <- get.q(X = X, l = l)
  l <- as.numeric(rownames(q))

# model EMs and set limits ---------------------------------------------------------------

  em.pot <- model.EM(X = X, q = q, l = l, plot = F)

  limits <- get.limits(loadings = em.pot)

# get robust EMs ----------------------------------------------------------

  em.rob <- robust.EM(em = em.pot,classunits = phi,limits = limits,
                      plot = F,
                      legend = "topright",
                      cex = 1.2,
                      cex.axis = 2,
                      colour = brewer.pal(length(limits)/2, 'Set1'),
                      median = TRUE,
                      mc_n = 1000)


# write datageo.csv file ------------------------------------------------------


  X1 <-as_tibble(matrix(em.rob$scores$mean,ncol = 5, dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5"))), .name_repair = "unique") %>%
    rename(EM1=V1, EM2=V2,EM3=V3, EM4=V4,EM5=V5)
  X2 <-as_tibble(matrix(em.rob$scores$sd,ncol = 5, dimnames = list(NULL, c("V1", "V2", "V3", "V4", "V5"))), .name_repair = "unique") %>%
    rename(EM1sd=V1, EM2sd=V2,EM3sd=V3, EM4sd=V4,EM5sd=V5)


  if(!file.exists("analysis/data/derived_data/datageo.csv")){
    datageo <- bind_cols(X0,X1, X2)
    path_out <- here::here("analysis/data/derived_data/","datageo.csv")
    write_csv(datageo,path_out)
  }


}


# 2. Create Figure 2 stratigraphic plot -----------------------------------------------------

plot_stratplot <- function(path) {

  # Load data ---------------------------------------------------------------
  geodata <- read_csv(path, col_types =cols())

  # lengthens data --------------------------------------------------------

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


  # get zone  ---------------------------------------------------------------

  zone_data <- tibble(ymin = c(38,140,240),
                      ymax = c(50,170,268),
                      xmin = -Inf, xmax = Inf)  ###### Stratiplot

  # Cluster plot ------------------------------------------------------------

  coniss <- geodata_longer %>% ###### Cluster plot
    nested_data(qualifiers = Depth, key = values, value = count, trans = scale) %>%
    nested_chclust_coniss()

  # Geochemical stratigraphic plots  -------------------------------------------------------

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
      units = c("d13C" = "‰", "d18O" = "‰"),
      default_units = "%") +
    labs(x = NULL, y = "Depth (cm)") +
    scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
    theme_set(theme_paleo(8)) +
    theme(
      text = element_text(size=8),
      axis.ticks = element_line(colour = "grey70", size = 0.3),
      panel.grid.major =element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", colour = "grey50")
    )

  wrap_plots(
    alta_plot +
      theme(strip.background = element_blank(), strip.text.y = element_blank()),
    ggplot() +
      layer_zone_boundaries(coniss, aes(y = Depth)) +
      layer_dendrogram(coniss, aes(y = Depth), param = "CONISS") +
      scale_x_continuous(breaks = scales::breaks_extended(n = 3)) +
      theme(axis.text.y.left = element_blank(),
            axis.ticks.y.left = element_blank(),
            text = element_text(size=8),
            panel.background = element_rect(fill = "white", colour = "grey50"))+
      labs(x = "coniss", y = NULL),
    nrow = 1,
    widths = c(8, 0.8)
  )


# Export plot -------------------------------------------------------------

  #  pdf_out <- here::here("analysis/figures","Fig2.pdf")
  #  png_out <- here::here("analysis/figures","Fig2.png")

  #  ggsave(pdf_out, width = 25,height = 12, units = 'cm',device = cairo_pdf)
  #  ggsave(png_out,  width = 25,height = 12, units = 'cm',device = "png")

}
