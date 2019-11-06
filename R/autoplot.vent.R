#' autoplot.vent
#'
#' autoplot.vent plots a vent dataframe
#'
#' @param dat a vent dataframe.
#' @param fsave if TRUE the function saves the figure in a pdf, otherwise returns a fig
#' @return plot.
#' @import ggplot2
#' @method vent data.frame
#' @aliases autoplot
#' @usage autoplot.vent(dat, fsave = TRUE)
#' @export autoplot.vent
autoplot.vent <-function(dat, fsave = TRUE){

  if (!"vent" %in% class(dat)) stop("Object is not of class vent")

  title <- paste(as.character(dat$cpu_date[1]), dat$subj[1], dat$drug[1], dat$dose[1], dat$unit[1], sep = " ")
  file_name <- paste(as.character(dat$cpu_date[1]), dat$subj[1], dat$drug[1], dat$dose[1], sep = "_")
  dat$int_min <- as.numeric(dat$int_min)

  labs <- as.numeric(seq(min(dat$int_min), max(dat$int_min), 15))

  fig <- ggplot(dat, aes_string('int_min', 'mean', col = 'measure')) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    scale_x_continuous(labels = labs, breaks = labs) +
    facet_wrap(vars(measure), ncol = 4, scales = "free_y") +
    labs(title = title, y = "mean +/- stdev", x = "minutes") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)
    )
  if (fsave == TRUE) {
    ggsave(paste0(file_name, ".pdf"), fig, device = "pdf", width = 30, height = 30, units = "cm")
  } else {
    return(fig)
  }
}
