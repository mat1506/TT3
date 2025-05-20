# R/REMMA_comp.R

#' Preprocess REMMA sediment and grain size data
#'
#' @param path_x0 path to CSV file with H01 data
#' @param path_tte path to TTGmicro CSV (grain size matrix)
#' @return A list with elements H01_data (tibble) and gs_matrix (matrix)
#' @importFrom readr read_csv cols
#' @importFrom dplyr mutate_at vars
#' @importFrom tibble tibble as_tibble
#' @importFrom tidyr
#' @export
load_remma_data <- function(path_x0, path_tte) {
  # Read H01 sediment data
  H01_data <- readr::read_csv(path_x0, col_types = cols())

  # Read grain size data and convert to matrix
  df <- readr::read_csv(path_tte, col_types = cols(), col_names = TRUE) %>%
    dplyr::mutate_at(dplyr::vars(depth), as.factor)
  gs_matrix <- as.matrix(tibble::column_to_rownames(df, var = "depth"))

  list(
    H01_data = H01_data,
    gs_matrix = gs_matrix
  )
}

#' Perform environmental mode analysis using EMMAgeo
#'
#' @param gs_matrix grain size matrix (rows=records, cols=phi classes)
#' @param max_corr numeric maximum correlation threshold
#' @param n number of potential EMs
#' @param plot logical, whether to plot intermediate results
#' @return A list with EM potential model, robust loadings, and scores
#' @importFrom EMMAgeo get.l get.q model.EM get.limits robust.loadings robust.EM
#' @importFrom RColorBrewer brewer.pal
#' @export
run_remma_analysis <- function(gs_matrix,
                               max_corr = 0.95,
                               n = 20,
                               plot = TRUE) {
  # Compute l and q
  l_param <- EMMAgeo::get.l(X = gs_matrix, max = max_corr, n = n)
  q_param <- EMMAgeo::get.q(X = gs_matrix, l = l_param)
  # Potential EM model
  em_pot <- EMMAgeo::model.EM(X = gs_matrix, q = q_param, l = as.numeric(rownames(q_param)), plot = plot)
  # Limits for robust loadings
  limits <- EMMAgeo::get.limits(loadings = em_pot)
  # Robust loadings and EMs
  robust_load <- EMMAgeo::robust.loadings(em = em_pot, limits = limits, plot = plot)
  em_rob <- EMMAgeo::robust.EM(em = em_pot,
                               classunits = as.numeric(colnames(gs_matrix)),
                               limits = limits,
                               plot = plot,
                               legend = "topright",
                               cex = 1.2,
                               l = "mRn",
                               cex.axis = 2,
                               colour = RColorBrewer::brewer.pal(length(limits)/2, 'Set1'),
                               median = TRUE,
                               mc_n = 100)
  # Return results
  list(
    em_potential = em_pot,
    limits = limits,
    robust_loadings = robust_load,
    em_rob = em_rob
  )
}

#' Export REMMA analysis scores to CSV
#'
#' @param em_rob object output from run_remma_analysis()$em_rob
#' @param out_path path to write the combined scores tibble
#' @export
export_remma_scores <- function(em_rob, out_path) {
  # Extract mean and sd scores
  X1 <- tibble::as_tibble(em_rob$scores$mean) %>%
    dplyr::rename_with(~ paste0("EM", seq_along(.)), everything())
  X2 <- tibble::as_tibble(em_rob$scores$sd) %>%
    dplyr::rename_with(~ paste0("EM", seq_along(.), "sd"), everything())
  datageo <- dplyr::bind_cols(X1, X2)
  readr::write_csv(datageo, out_path)
}

