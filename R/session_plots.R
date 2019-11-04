#' session_plots.
#'
#' session_plots create plots from objects returned by summarize_vent
#'
#' @param dat a iox dataframe.
#' @param inter logical that determines if dialogs will be used.
#' @param path where to save the files.
#' @param baseline length of baseline in minutes.
#' @param bin length of bin in minutes.
#' @return plots
#' @import ggplot2
#' @export
#'
session_plots <-function(dat, inter = TRUE, path, baseline = 30, bin = 3) {

  dfs <- summarize_vent(dat = dat, baseline = baseline, bin = bin, inter = FALSE)
  dat_vent <- dfs$dat_vent

  if(inter == TRUE) {
    svDialogs::dlg_message("Saving Plots: please choose a folder.", type = "ok")
    path <- svDialogs::dlg_dir()$res
  }

  setwd(path)

  lapply(split.data.frame(dat_vent, dat_vent$subj), function(x){
    autoplot(x)
    }
  )

}




