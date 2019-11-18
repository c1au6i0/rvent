#' autoplot.vent
#'
#' autoplot.vent plots a vent dataframe
#'
#' @param dat a vent dataframe.
#' @param vent_stat stat used to summarize bins displayed in the y axis ("median" or "mean").
#' @param measure metric to display.
#' @param fsave if TRUE the function saves the figure in a pdf, otherwise returns a fig.
#' @return plot.
#' @import ggplot2
#' @importFrom rlang .data
#' @method vent data.frame
#' @aliases autoplot
#' @usage autoplot.vent(dat, vent_stat = "mean", fsave = TRUE, measure = "ALL")
#' @export autoplot.vent
autoplot.vent <- function(dat, vent_stat = "mean", fsave = TRUE, measure = "ALL") {
  if (!"vent" %in% class(dat)) stop("Object is not of class vent")

  if (length(measure) == 1) {
    if (measure != "ALL") {
      dat <- dat[dat$measure %in% measure, ]
    }
  } else {
    dat <- dat[dat$measure %in% measure, ]
  }
  title <- paste(as.character(dat$cpu_date[1]), dat$subj[1], dat$drug[1], dat$dose[1], dat$unit[1], sep = " ")
  file_name <- paste(as.character(dat$cpu_date[1]), dat$subj[1], dat$drug[1], dat$dose[1], sep = "_")
  dat$int_min <- as.numeric(dat$int_min)

  labs <- as.numeric(seq(min(dat$int_min), max(dat$int_min), 15))

  fig <-
    ggplot(dat, aes_string("int_min", vent_stat, col = "measure")) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd)) +
    geom_vline(xintercept = 0.5, lty = 4, lwd = 0.5) +
    scale_x_continuous(labels = labs, breaks = labs) +
    labs(title = title, y = paste(vent_stat, "+/- stdev"), x = "minutes") +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    )


  if (length(measure) == 1) {
    if (measure == "ALL") {
      fig <- fig +
        facet_wrap(vars(measure), ncol = 4, scales = "free_y")
    }
  } else {
    fig <- fig +
      facet_wrap(vars(measure), scales = "free_y")
  }



  if (fsave == TRUE) {
    ggsave(paste0(file_name, ".pdf"), fig, device = "pdf", width = 30, height = 30, units = "cm")
  } else {
    return(fig)
  }
}
