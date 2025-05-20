## R/load_data.R

#' Load and preprocess all raw data
#'
#' @param raw_path character path to the raw_data folder
#' @return A list with elements diatom, palyno, phyto, zone_data
#' @importFrom dplyr if_else
#' @importFrom readr read_csv cols
#' @importFrom dplyr select mutate group_by if_else
#' @importFrom dplyr select mutate group_by summarise
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom tibble tibble
#' @export
load_and_prep_data <- function(raw_path = system.file("testdata", package = "TT3")) {
  # Diatoms
  diatom_file <- file.path(raw_path, "TTGdiatom2.csv")
  diatom <- readr::read_csv(diatom_file, col_types = cols()) %>%
    dplyr::select(-3:-6)
  diatom_long <- diatom %>%
    tidyr::pivot_longer(-taxon:-ecological_group,
                        names_to  = "depth",
                        values_to = "abn") %>%
    dplyr::mutate(
      depth = as.numeric(depth),
      abn   = dplyr::if_else(is.na(abn), 0, abn)
    ) %>%
    dplyr::group_by(depth) %>%
      dplyr::mutate(abn_p = abn / sum(abn) * 100)

  # Palynomorphs
  palyno_file <- file.path(raw_path, "TTGpalynomorphs.csv")
  palyno <- readr::read_csv(palyno_file, col_types = cols()) %>%
    tidyr::pivot_longer(-taxon, names_to = "depth", values_to = "conc") %>%
    dplyr::mutate(
      depth = as.numeric(depth),
      conc  = dplyr::if_else(is.na(conc), 0, conc)
    ) %>%
    dplyr::group_by(depth) %>%
    dplyr::mutate(conc_p = conc / sum(conc) * 100)

  # Phytoliths
  phyto_file <- file.path(raw_path, "PhytolitsTT3.csv")
  phyto <- readr::read_csv(phyto_file, col_types = cols()) %>%
    dplyr::slice(-1:-4) %>%
    tidyr::pivot_longer(-depth, names_to = "taxon", values_to = "abn") %>%
    dplyr::mutate(
      depth = as.numeric(depth),
      abn   = dplyr::if_else(is.na(abn), 0, abn)
    ) %>%
    dplyr::group_by(depth) %>%
    dplyr::mutate(abn_p = abn / sum(abn) * 100)

  # Zone data for shading
  zone_data <- tibble::tibble(
    ymin = c(150, 158, 170),
    ymax = c(155, 165, 185),
    xmin = -Inf,
    xmax =  Inf
  )

  list(
    diatom    = diatom_long,
    palyno    = palyno,
    phyto     = phyto,
    zone_data = zone_data
  )
}
