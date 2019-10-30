#' session_plots.
#'
#' session_plots create plotz of iox objects
#'
#' @param dat a iox dataframe.
#' @param path where to save the files
#' @param baseline length of baseline in minutes
#' @param bin length of bin in minutes
#' @param fsave if
#' @return plots
#' @import ggplot2
#' @export
#'
session_plots <-function(dat, path, baseline = 30, bin = 3) {

  dfs <- summarize_vent(dat = dat, baseline = baseline, bin = bin)
  dat_vent <- dfs$dat_vent
  setwd(path)

  lapply(split.data.frame(dat_vent, dat_vent$subj), function(x){
    class(x) <- c("vent", "data.frame")
    autoplot(x)
    }
  )

}




