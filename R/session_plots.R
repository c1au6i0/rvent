#' session_plots.
#'
#' session_plots creates plots from list of vent dataframes.
#'
#' @param dat a list of  dataframes as returned by \code{\link{import_session}}.
#' @param inter logical that determines if dialogs will be used.
#' @param path where to save the files.
#' @param vent_stat stat used to summarize bins displayed in the y axis ("median" or "mean").
#' @param bin length of bin in minutes.
#' @param measure metric to display.
#' @param filter_vals if TRUE filters data to eliminte impossible value for rat phisiology.
#' @param fsave if TRUE saves plots in pdf files in path.
#' @return plots
#' @import ggplot2
#' @export
#'
session_plots <- function(dat,
                          inter = TRUE,
                          path,
                          vent_stat = "mean",
                          bin = 3, fsave = TRUE,
                          measure = "ALL",
                          filter_vals = TRUE) {
  dfs <- summarize_vent(dat = dat, bin = bin, inter = FALSE, filter_vals = filter_vals)
  dat_vent <- dfs$dat_vent

  if (inter == TRUE) {
    svDialogs::dlg_message("Saving Plots: please choose a folder.", type = "ok")
    path <- svDialogs::dlg_dir()$res
    setwd(path)
  }

  l_subj <- split.data.frame(dat_vent, list(dat_vent$subj, dat_vent$cpu_date), drop = TRUE)

  if (fsave == TRUE) {
    lapply(l_subj, function(x) {
      autoplot(x, fsave = TRUE, measure = measure)
    })
  } else {
    figs <-
    lapply(l_subj,function(x) {
      autoplot(x, vent_stat = vent_stat, fsave = FALSE, measure = measure)
    })
    return(figs)
  }
}
