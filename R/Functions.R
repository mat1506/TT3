

# 1. setup data

data_setup <- function(tt){
  mnidata<- pivot_longer(tt,cols =TC:sand, names_to = "geoche", values_to = "abund")

}


# 2. functions targets descriptive

mydescriptive <- function(dt, DV, IV){
  zone_data <- tibble(ymin = c(18,32,62,81), ymax = c(27,41,73,90), xmin = -Inf, xmax = Inf)

  min_plot <- ggplot(minalk, aes(x = abn, y = depth)) +
    geom_col_segsh() +
    geom_lineh() +
    geom_rect(mapping = aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
              data = zone_data,
              alpha = 0.4,
              fill = "coral4",inherit.aes = FALSE )+
    scale_y_reverse(breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
    facet_geochem_gridh(vars(alk)) +
    labs(x = "Relative abundance (%)", y = "Depth (cm)")
  p <- min_plot +
    layer_dendrogram(coniss, aes(y = depth), alk = "CONISS") +
    layer_zone_boundaries(coniss, aes(y = depth))
}

# 3. functions targets model


mymodel <- function(dt){

}
