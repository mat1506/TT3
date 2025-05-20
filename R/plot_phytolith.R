## R/plot_phytolith.R

#' Plot phytolith groups
#'
#' @param phyto_df tibble from load_and_prep_data()$phyto
#' @param zone_data tibble from load_and_prep_data()$zone_data
#' @return A ggplot2 object
#' @import ggplot2
#' @export
plot_phytolith <- function(phyto_df, zone_data) {
  ggplot2::ggplot(phyto_df, aes(x = abn_p, y = depth)) +
    geom_col_segsh(aes(colour = taxon), size = 1.5, show.legend = FALSE) +
    scale_color_grey(start = 0.7, end = 0.1) +
    scale_y_reverse(breaks = seq(0, 260, by = 20)) +
    geom_rect(data = zone_data,
              aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    facet_abundanceh(vars(taxon), scales = "free", space = "fixed", shrink = FALSE) +
    labs(x = NULL, y = NULL) +
    theme_paleo(8) +
    ggtitle("Phytolith groups")
}
