# R/plots.R

#' Plot diatom group abundances
#'
#' @param diatom_data tibble resultado de tu procesamiento (taxon.sel.lon)
#' @param zone_data tibble con las zonas a sombrear (ymin, ymax, xmin, xmax)
#' @return ggplot object
#' @import ggplot2
#' @importFrom ggpubr ggarrange
#' @export
plot_diatom_groups <- function(diatom_data, zone_data) {
  ggplot(diatom_data, aes(x = abn_p, y = depth)) +
    geom_col_segsh(aes(colour = taxon, size = 1.5), show.legend = FALSE) +
    scale_color_grey(start = 0.7, end = 0.1) +
    scale_y_reverse(breaks = seq(0, 260, by = 20)) +
    geom_rect(
      aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
      data = zone_data, alpha = 0.2, fill = "red", inherit.aes = FALSE
    ) +
    facet_abundanceh(vars(taxon), scales = "free", space = "fixed", shrink = FALSE) +
    labs(x = NULL, y = NULL) +
    theme(
      text            = element_text(size = 18),
      axis.ticks      = element_line(colour = "grey50", linewidth = 0.15),
      axis.title      = element_text(size = rel(0.85), face = "bold"),
      axis.text       = element_text(size = rel(0.70), face = "bold"),
      axis.text.x.bottom = element_text(angle = 0, hjust = 1),
      axis.line       = element_line(color = "black", linewidth = 0.15),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    ggtitle("Diatoms groups")
}
