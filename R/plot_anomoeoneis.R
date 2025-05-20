## R/plot_anomoeoneis.R

#' Plot Anomoeoneis sculpa abundance
#'
#' @param diatom_df tibble from load_and_prep_data()$diatom
#' @param zone_data tibble from load_and_prep_data()$zone_data
#' @return A ggplot2 object
#' @import ggplot2
#' @export
plot_anomoeoneis <- function(diatom_df, zone_data) {
  df <- diatom_df %>%
    tidyr::pivot_wider(names_from = taxon, values_from = abn) %>%
    dplyr::select(depth, Anomoeoneis_sphaerophora) %>%
    dplyr::rename(`Anomoeoneis sculpa` = Anomoeoneis_sphaerophora) %>%
    tidyr::pivot_longer(-depth, names_to = "taxon", values_to = "abn") %>%
    dplyr::group_by(depth) %>%
    dplyr::mutate(abn_p = abn / sum(abn) * 100)

  ggplot2::ggplot(df, aes(x = abn_p, y = depth)) +
    geom_col_segsh(aes(colour = taxon), size = 1.5, show.legend = FALSE) +
    scale_color_grey(start = 0.7, end = 0.1) +
    scale_y_reverse(breaks = seq(40, 260, by = 10)) +
    geom_rect(data = zone_data,
              aes(ymin = ymin, ymax = ymax, xmin = xmin, xmax = xmax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    facet_abundanceh(vars(taxon), scales = "free", space = "fixed") +
    labs(x = NULL, y = "Depth (cm)") +
    theme_paleo(8) +
    ggtitle("Anomoeoneis sculpa")
}

