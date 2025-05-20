## R/geochronR.R

#' Load a LiPD dataset from a folder
#'
#' @param folder Path to the LiPD .lpd folder
#' @return A LiPD object
#' @importFrom lipdR readLipd
#' @export
load_lpd <- function(folder) {
  lipdR::readLipd(folder)
}

#' Run Bacon and Bchron age models
#'
#' @param lpd A LiPD object
#' @param bacon_dir Directory to store Bacon output
#' @param bacon_args List of additional arguments for runBacon
#' @param bchron_args List of additional arguments for runBchron
#' @return A LiPD object with added age models
#' @importFrom geoChronR runBacon runBchron
#' @export
run_age_models <- function(lpd,
                           bacon_dir,
                           bacon_args = list(
                             bacon.thick = 3,
                             lab.id.var = 'labid',
                             model.num = 1,
                             age.var = 'age',
                             age.uncertainty.var = 'ageuncertainty',
                             depth.var = 'depth',
                             reservoir.age.14c.var = 'dR',
                             reservoir.age.14c.uncertainty.var = 'dSTD',
                             accept.suggestions = TRUE,
                             cc = 3,
                             ssize = 2000,
                             d.min = 38,
                             d.max = 266,
                             bacon.acc.mean = c(70,100),
                             acc.shape = c(1.3,1.6),
                             mem.mean = 0.4,
                             hiatus.depths = 165,
                             hiatus.max = 500,
                             ask = FALSE
                           ),
                           bchron_args = list(
                             iter = 40000,
                             model.num = 2,
                             predictPositions = seq(38,266,by = 1),
                             lab.id.var = 'labid',
                             age.var = 'age',
                             age.uncertainty.var = 'ageuncertainty',
                             depth.var = 'depth',
                             reservoir.age.14c.var = 'dR',
                             reservoir.age.14c.uncertainty.var = 'dSTD',
                             cal.curves = 'shcal20'
                           )) {
  if (!dir.exists(bacon_dir)) {
    dir.create(bacon_dir, recursive = TRUE)
  }
  # Run Bacon
  do.call(geoChronR::runBacon, c(list(lpd = lpd, bacon.dir = bacon_dir), bacon_args))
  # Run Bchron
  do.call(geoChronR::runBchron, c(list(lpd = lpd), bchron_args))
}

#' Extract ensemble data for a given model
#'
#' @param lpd A LiPD object with run models added
#' @param model.num The model number (1 for Bacon, 2 for Bchron)
#' @return A tibble with columns depth and ageEnsemble
#' @importFrom geoChronR selectData
#' @export
get_ensemble <- function(lpd, model.num) {
  age_df  <- geoChronR::selectData(lpd,
                                   var.name = 'ageEnsemble',
                                   paleo.or.chron = 'chronData',
                                   model.num = model.num,
                                   table.type = 'ensemble')
  depth_df <- geoChronR::selectData(lpd,
                                    var.name = 'depth',
                                    paleo.or.chron = 'chronData',
                                    model.num = model.num,
                                    table.type = 'ensemble')
  tibble::tibble(
    depth = depth_df[[1]],
    ageEnsemble = age_df[[1]]
  )
}

#' Plot ensemble age-depth ribbons for multiple models
#'
#' @param ensembles A named list of tibbles from get_ensemble
#' @param colors A named vector of colors for each model
#' @return A ggplot object
#' @import ggplot2
#' @export
plot_age_ensemble <- function(ensembles, colors = c(Bacon = 'red', Bchron = 'blue')) {
  plt <- NULL
  for (n in names(ensembles)) {
    df <- ensembles[[n]]
    color <- colors[n]
    p <- ggplot2::ggplot(df, aes(x = ageEnsemble, y = depth)) +
      ggplot2::geom_ribbon(aes(xmin = ageEnsemble - sd(ageEnsemble),
                               xmax = ageEnsemble + sd(ageEnsemble)),
                           fill = color, alpha = 0.3) +
      ggplot2::geom_line(color = color)
    if (is.null(plt)) {
      plt <- p
    } else {
      plt <- plt + p
    }
  }
  plt + ggplot2::scale_y_reverse() + ggplot2::labs(x = 'Age', y = 'Depth')
}

