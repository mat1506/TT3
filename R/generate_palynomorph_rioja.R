# R/generate_palynomorph_rioja.R

#' Generate palynomorph Rioja diagram and save output
#'
#' @param raw_path Path to folder containing `TTGpalynomorphsRioja.csv`
#' @param output_dir Directory to save plot (default: analysis/supplement)
#' @export
generate_palynomorph_rioja <- function(
    raw_path   = here::here("analysis/data/raw_data"),
    output_dir = here::here("analysis/supplement")
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  pal_path <- file.path(raw_path, "TTGpalynomorphsRioja.csv")
  pal_df   <- readr::read_csv(pal_path, col_types = readr::cols())
  taxa     <- pal_df[-(1:6)]
  chron    <- pal_df[1:6]

  options(scipen = 10)
  cl <- rioja::chclust(dist(sqrt(taxa)))
  chron$Zone <- stats::cutree(cl, k = 3)
  zones <- dplyr::group_by(chron, Zone) %>%
    dplyr::summarise(zm = mean(age)) %>%
    dplyr::mutate(name = paste("Zone", Zone)) %>%
    dplyr::select(-Zone)

  ylab <- expression(Age~"("^{14}*C~years~BP*")")
  p2 <- riojaPlot::riojaPlot(
    taxa, chron,
    scale.percent    = FALSE,
    ytks1            = seq(6000,20000,1000),
    yvar.name        = "age",
    ylabel           = ylab,
    labels.break.n   = 15,
    labels.italicise = TRUE,
    srt.xlabel       = 45,
    ymin             = 6000,
    ymax             = 20000,
    col.line         = "blue",
    col.poly         = "blue",
    wa.order         = "bottomleft",
    plot.exag        = TRUE,
    cex.xlabel       = 0.7,
    xRight           = 0.82
  ) %>%
    riojaPlot::addRPZoneNames(zones, xRight = 0.9, cex = 0.6) %>%
    riojaPlot::addRPClustZone(cl, col = "red") %>%
    riojaPlot::addRPClust(cl)

  ggplot2::ggsave(
    file.path(output_dir, "palynomorphs_rioja.png"),
    plot = p2, width = 8, height = 6
  )
  invisible(p2)
}

