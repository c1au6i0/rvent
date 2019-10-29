#' summarize vent dataframes.
#'
#' summarize_vent_in is the non-interactive version of \code{\link{summarize_vent}} and it is used internaly by plot_vent creates duration intervals, filters and summarize a list of vent dataframes.
#'
#' @param dat a list of vent dataframes as returned by import_session.
#' @param basel length of baseline in minutes
#' @param bnin length of bins in minutes
#' @return a list of dataframes including one of class vent
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export

summarize_vent_ni <- function(dat, basel = 30, bin = 3){

  dat2 <- lapply(dat, find_bins, baseline = baseline, bin = bin)

  suppressWarnings(dat_all <- dplyr::bind_rows(dat2))

  col_melt <- which(names(dat_all) %in% c("Ti_msec", "Sr_per"))

  # wide to long
  dat_long <- data.table::melt(dat_all, measure.vars = col_melt[1]:col_melt[2],
                               variable.name = "measure", value.name = "value")

  # first data summary
  dat_sm <- dat_long %>%
    dplyr::group_by(.data$cpu_date, .data$subj_drug, .data$dose, .data$unit, .data$int_min, .data$measure) %>%
    dplyr::summarise(mean = mean(.data$value),
                     median = median(.data$value),
                     sd = sd(.data$value),
                     n = dplyr::n()) %>%
    tidyr::separate(.data$subj_drug, c("subj", "drug"), remove = TRUE)

  class(dat_sm) <- c("vent", "data.frame")

  # data summary longer
  dat_sm2  <- data.table::melt(dat_sm, measure.vars = c("mean", "median", "sd", "n"),
                              variable.name = "stat", value.name = "value") %>%
             dplyr:: arrange(as.numeric(.data$int_min), .data$stat) %>%
             tidyr::unite("int_stat", .data$int_min, .data$stat, sep = "_")

  # reorder levels
  dat_sm2$int_stat <- factor(dat_sm2$int_stat, levels = unique(dat_sm2$int_stat))

  dat_sm2 <- data.table::dcast(dat_sm2,
                               cpu_date + subj + drug + dose + unit +  measure ~ int_stat, value.var = "value")

  # Split and long to wide again
  suppressWarnings(dat_fs <- split.data.frame(dat_sm2, dat_sm$measure))

  # dat_sm is the datgrame used to make plots

  return(list(dat_sm = dat_sm, dat_sm2 = dat_sm2, dat_long = dat_long, dat_fs = dat_fs))

}

