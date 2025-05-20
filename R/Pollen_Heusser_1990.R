#' Generate Heusser (1990) Pollen Diagrams and Save Outputs
#'
#' This function reads the Heusser (1990) pollen count dataset,
#' extracts metadata and tabular data, writes them to disk,
#' and produces two Rioja pollen diagrams, saving each to PNG files
#' under the `analysis/supplement/` folder.
#'
#' @param raw_path Character. Path to `analysis/data/raw_data/` containing:
#'   - `Tagua_Tagua_CLAM_age_model_pollen_count.tab`
#'   - `Tagua_Tagua_CLAM_age_model_pollen_count_data.csv`
#'   - `TTGpalynomorphsRioja.csv`
#' @param output_dir Character. Directory where to save metadata, CSV, and plots.
#'   Defaults to `analysis/supplement/`.
#' @return Invisibly, list of two ggplot objects (p1, p2).
#' @importFrom rioja chclust
#' @importFrom riojaPlot riojaPlot addRPZoneNames addRPClustZone addRPClust
#' @importFrom readr read_csv cols
#' @importFrom dplyr group_by summarise mutate
#' @importFrom ggplot2 ggsave
#' @importFrom magrittr %>%
#' @export
Pollen_Heusser_1990 <- function(
    raw_path    = here::here("analysis/data/raw_data"),
    output_dir  = here::here("analysis/supplement")
) {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  ## 1. Extract metadata + write CSV
  tab_path <- file.path(raw_path, "Tagua_Tagua_CLAM_age_model_pollen_count.tab")
  lines <- readLines(tab_path)
  meta_lines <- 1:81
  metadata   <- lines[meta_lines]
  data_lines <- lines[-meta_lines]
  writeLines(metadata, file.path(output_dir, "Tagua_Tagua_metadata.txt"))

  df <- read.table(
    text       = paste(data_lines, collapse = "\n"),
    sep        = "\t",
    header     = TRUE,
    fill       = TRUE,
    check.names= FALSE,
    comment.char = ""
  )
  write.csv(df, file.path(output_dir, "Tagua_Tagua_data.csv"), row.names = FALSE)

  ## 2. First pollen diagram
  poll_path <- file.path(raw_path,
                         "Tagua_Tagua_CLAM_age_model_pollen_count_data.csv")
  poll_df   <- readr::read_csv(poll_path, col_types = cols())
  ma.pollen <- poll_df[-(1:6)]
  chron     <- poll_df[1:6]
  # Convert to percentages
  pct_df <- ma.pollen
  pct_df[] <- lapply(ma.pollen, function(x) x / sum(x, na.rm = TRUE) * 100)
  pct_df[] <- lapply(ma.pollen, function(x) x / sum(x, na.rm = TRUE) * 100)
  # Filter taxa with total abundance >15%
  sums <- colSums(pct_df, na.rm = TRUE)
  keep <- sums > 15
  poll1 <- pct_df[, keep, drop = FALSE]
  ytks1 <- seq(10, 45, by = 1)
  ylab1 <- expression(Age~"("^{14}*C~ka~BP*")")
  clust1 <- chclust(dist(sqrt(poll1)))
  chron$Zone <- cutree(clust1, k = 3)
  zones1 <- chron %>%
    group_by(Zone) %>%
    summarise(zm = mean(age)) %>%
    mutate(name = paste("Zone", Zone)) %>%
    dplyr::select(-Zone)

  p1 <- riojaPlot(
    poll1, chron,
    scale.percent   = TRUE,
    ytks1           = ytks1,
    yvar.name       = "age",
    ylabel          = ylab1,
    labels.break.n  = 15,
    labels.italicise= TRUE,
    srt.xlabel      = 45,
    ymin            = 11,
    wa.order        = "bottomleft",
    plot.clust      = TRUE,
    plot.exag       = TRUE,
    cex.xlabel      = 0.7,
    xRight          = 0.82
  ) %>%
    addRPZoneNames(zones1, xRight = 0.9, cex = 0.6) %>%
    addRPClustZone(clust1, col = "red") %>%
    addRPClust(clust1)

  ggsave(
    filename = file.path(output_dir, "Pollen_Heusser1990_plot1.png"),
    plot     = p1,
    width    = 8,
    height   = 6
  )

  ## 3. Second palynomorph diagram
  pal_path <- file.path(raw_path, "TTGpalynomorphsRioja.csv")
  pal_df   <- readr::read_csv(pal_path, col_types = cols())
  ma2      <- pal_df[-(1:6)]
  chron2   <- pal_df[1:6]
  ytks2    <- seq(6000, 20000, by = 1000)
  ylab2    <- expression(Age~"("^{14}*C~years~BP*")")
  options(scipen = 10)
  clust2   <- chclust(dist(sqrt(ma2)))
  chron2$Zone <- cutree(clust2, k = 3)
  zones2   <- chron2 %>%
    group_by(Zone) %>%
    summarise(zm = mean(age)) %>%
    mutate(name = paste("Zone", Zone)) %>%
    dplyr::select(-Zone)

  p2 <- riojaPlot(
    ma2, chron2,
    scale.percent   = FALSE,
    ytks1           = ytks2,
    yvar.name       = "age",
    ylabel          = ylab2,
    labels.break.n  = 15,
    labels.italicise= TRUE,
    srt.xlabel      = 45,
    ymin            = 6000,
    ymax            = 20000,
    col.line        = "blue",
    col.poly        = "blue",
    wa.order        = "bottomleft",
    plot.exag       = TRUE,
    cex.xlabel      = 0.7,
    xRight          = 0.82
  ) %>%
    addRPZoneNames(zones2, xRight = 0.9, cex = 0.6) %>%
    addRPClustZone(clust2, col = "red") %>%
    addRPClust(clust2)

  ggsave(
    filename = file.path(output_dir, "Pollen_Heusser1990_plot2.png"),
    plot     = p2,
    width    = 8,
    height   = 6
  )

  invisible(list(plot1 = p1, plot2 = p2))
}

