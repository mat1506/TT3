## R/figure4.R

#' Combine all stratigraphy plots into Figure 4
#'
#' @param data_list output of load_and_prep_data()
#' @return A patchwork object
#' @import patchwork
#' @export
plot_figure4 <- function(data_list) {
  p1 <- plot_diatom_pb(data_list$diatom, data_list$zone_data)
  p2 <- plot_anomoeoneis(data_list$diatom, data_list$zone_data)
  p3 <- plot_palynomorph(data_list$palyno, data_list$zone_data)
  p4 <- plot_phytolith(data_list$phyto, data_list$zone_data)

  top    <- p1 / p2
  bottom <- p3 | p4
  top / bottom + plot_annotation(tag_levels = "a")
}

#' Save Figure 4 to file
#'
#' @param filename path where to save the image
#' @param data_list output of load_and_prep_data()
#' @export
save_figure4 <- function(filename, data_list) {
  fig <- plot_figure4(data_list)
  ggsave(filename, fig, width = 14, height = 8)
}
