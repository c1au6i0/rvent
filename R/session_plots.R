#' session_plots.
#'
#' session_plots creates plots from list of vent dataframes.
#'
#' @param dat a iox dataframe.
#' @param inter logical that determines if dialogs will be used.
#' @param path where to save the files.
#' @param vent_stat stat used to summarize bins displayed in the y axis ("median" or "mean").
#' @param baseline length of baseline in minutes.
#' @param bin length of bin in minutes.
#' @return plots
#' @import ggplot2
#' @export
#'
session_plots <- function(dat, inter = TRUE, path, vent_stat = "mean", baseline = 30, bin = 3) {
  dfs <- summarize_vent(dat = dat, baseline = baseline, bin = bin, inter = FALSE)
  dat_vent <- dfs$dat_vent

  if (inter == TRUE) {
    svDialogs::dlg_message("Saving Plots: please choose a folder.", type = "ok")
    path <- svDialogs::dlg_dir()$res
    setwd(path)
  }


  if (inter == TRUE) {
    lapply(split.data.frame(dat_vent, dat_vent$subj), function(x) {
      autoplot(x, fsave = TRUE)
    })
  } else {
    figs <- lapply(split.data.frame(dat_vent, dat_vent$subj), function(x) {
      autoplot(x, vent_stat = vent_stat, fsave = FALSE)
    })
    return(figs)
  }
}
