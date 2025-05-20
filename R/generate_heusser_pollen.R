# R/generate_heusser_pollen.R

#' Generate Heusser (1990) pollen count diagram and save output
#'
#' @param raw_path Path to folder containing the .tab and .csv files
#' @param output_dir Directory to save metadata, CSV, and plot (default: analysis/supplement)
#' @export
generate_heusser_pollen <- function(
    raw_path   = here::here("analysis/data/raw_data"),
    output_dir = here::here("analysis/supplement")
) {
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Read raw tab
  tab_file <- file.path(raw_path, "Tagua_Tagua_CLAM_age_model_pollen_count.tab")
  lines    <- readLines(tab_file)
  metadata <- lines[1:81]
  data_txt <- lines[-(1:81)]

  # Write metadata and full CSV
  writeLines(metadata, file.path(output_dir, "Tagua_Tagua_metadata.txt"))
  df <- utils::read.table(
    text       = paste(data_txt, collapse = "\n"),
    sep        = "\t",
    header     = TRUE,
    fill       = TRUE,
    check.names= FALSE,
    comment.char=""
  )
  readr::write_csv(df, file.path(output_dir, "Tagua_Tagua_data.csv"))

  # Read percentage data
  pct_path <- file.path(raw_path, "Tagua_Tagua_CLAM_age_model_pollen_count_data.csv")
  poll_df  <- readr::read_csv(pct_path, col_types = readr::cols())
  taxa     <- poll_df[-(1:6)]
  chron    <- poll_df[1:6]

  # Convert to % and filter
  pct <- taxa
  pct[] <- lapply(taxa, function(x) x / sum(x, na.rm = TRUE) * 100)
  keep    <- colSums(pct, na.rm = TRUE) > 20
  poll    <- pct[, keep, drop = FALSE]

  # Cluster zones
  cl <- rioja::chclust(dist(sqrt(poll)))
  chron$Zone <- stats::cutree(cl, k = 3)
  zones <- dplyr::group_by(chron, Zone) %>%
    dplyr::summarise(zm = mean(age)) %>%
    dplyr::mutate(name = paste("Zone", Zone)) %>%
    dplyr::select(-Zone)

  # Plot
  ylab <- expression(Age~"("^{14}*C~ka~BP*")")
  p <- riojaPlot::riojaPlot(
    poll, chron,
    scale.percent    = TRUE,
    ytks1            = seq(10,45,1),
    yvar.name        = "age",
    ylabel           = ylab,
    labels.break.n   = 15,
    labels.italicise = TRUE,
    srt.xlabel       = 45,
    ymin             = 11,
    wa.order         = "bottomleft",
    plot.clust       = TRUE,
    plot.exag        = TRUE,
    cex.xlabel       = 0.7,
    xRight           = 0.82
  ) %>%
    riojaPlot::addRPZoneNames(zones, xRight = 0.9, cex = 0.6) %>%
    riojaPlot::addRPClustZone(cl, col = "red") %>%
    riojaPlot::addRPClust(cl)

  ggplot2::ggsave(
    file.path(output_dir, "heusser1990_pollen.png"),
    plot = p, width = 8, height = 6
  )
  invisible(p)
}
