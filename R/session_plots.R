#' session_plots.
#'
#' session_plots create plotz of iox objects
#'
#' @param dat a iox dataframe.
#' @param path where to save the files
#' @return plots
#' @import ggplot2
#' @export
#'

session_plots <-function(dat, path) {
  dat_vent <- summarize_vent_ni(dat = dat, basel = 30, bin = 3)
  dat_sm <- dat_vent[[1]]
  setwd(path)
  dat_sml <- split.data.frame(dat_sm, dat_sm$subj)

  lapply(split.data.frame(dat_sm, dat_sm$subj), function(x){
    class(x) <- c("vent", "data.frame")
    autoplot_vent(x)
    }
  )

}




