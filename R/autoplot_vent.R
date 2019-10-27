#' autoplot vent.
#'
#' autoplot_vent plot a dataframe of class vent
#'
#' @param dat a vent dataframe.
#' @return plot.
#' @import ggplot2
#' @export
#'

autoplot_vent <-function(dat){

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
    facet_wrap(vars(measure), ncol = 4, scale = "free_y") +
    labs(title = title, y = "mean +/- stdev", x = "minutes") +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)
    )
    ggsave(paste0(file_name, ".pdf"), fig, device = "pdf", width = 30, heigh = 30, units = "cm")
}
