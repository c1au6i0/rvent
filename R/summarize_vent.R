#' summarize _vent
#'
#' summarize_vent creates duration intervals, filters and summarizes a list of vent objects returned by import_vent.
#'
#' @param dat a list of vent dataframes as returned by import_session.
#' @param baseline length of baseline in minutes.
#' @param bin length of bins in minutes.
#' @param inter logical.
#' @param form stat for summarizing the data.
#' @param filter_vals if TRUE filters data to eliminte impossible value for rat phisiology.
#' @return a list of dataframes:
#' \enumerate{
#'   \item \strong{dat_long}: a dataframe of the session that contains a column *measure* and bins.
#'   \item \strong{dat_vent}: a vent dataframe, in which summary stats for each bin have been calculated.
#'   \item \strong{dat_sml}: a dataframe binned and summarized in which the summary stats have been melted in the column *int_stat*.
#'   \item \strong{dat_fs} a dat_sm dataframe splitted by *measure*.
#' }
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom data.table as.data.table
#' @import stats
#' @export
summarize_vent <- function(dat, inter = TRUE, baseline = 30, bin = 3, form = "mean", filter_vals = "TRUE") {
  if (inter == TRUE) {
    baseline_bin <- svDialogs::dlg_input("Insert the baseline and the bin duration in minutes, separated by a space")$res
    baseline_bin <- as.numeric(unlist(strsplit(baseline_bin, " ")))
    baseline <- baseline_bin[1]
    bin <- baseline_bin[2]
  }

  dat2 <- lapply(dat, find_bins, bin = bin, filter_vals = filter_vals)

  suppressWarnings(dat_all <- dplyr::bind_rows(dat2))

  col_melt <- which(names(dat_all) %in% c("Ti_msec", "Sr_per"))

  # wide to long
  dat_long <- data.table::melt(as.data.table(dat_all),
    measure.vars = col_melt[1]:col_melt[2],
    variable.name = "measure", value.name = "value"
  )
  dat_long <- as.data.frame(dat_long)

  # are there 2 drugs given to the same animal?
  if (all(c("drug.x", "drug.y") %in% names(dat_long))) {
    names(dat_long)[names(dat_long) == "drug.x"] <- "drug"
    dat_long <- dat_long[, names(dat_long) != "drug.y"]
  }

  # first data summary
  dat_vent <- dat_long %>%
    dplyr::group_by(.data$cpu_date, .data$subj, .data$drug, .data$dose, .data$unit, .data$int_min, .data$measure) %>%
    dplyr::summarise(
      mean = mean(.data$value),
      median = median(.data$value),
      sd = sd(.data$value),
      n = dplyr::n(),
      baseline = baseline,
      bin = bin
    )


  class(dat_vent) <- c("data.frame", "vent")

  if (inter == TRUE) {
    form <- svDialogs::dlg_list(list("mean", "median", "n", "sd"),
      multiple = TRUE,
      title = "Choose how to summarize the values of each bin"
    )$res
  }

  # data summary longer
  suppressWarnings(dat_sm <- data.table::melt(as.data.table(dat_vent),
    measure.vars = c("mean", "median", "sd", "n"),
    variable.name = "stat", value.name = "value"
  ) %>%
    dplyr::filter(.data$stat %in% form) %>%
    dplyr::arrange(as.numeric(.data$int_min), .data$stat) %>%
    tidyr::unite("int_stat", .data$int_min, .data$stat, sep = "_"))

  # reorder levels
  dat_sm$int_stat <- factor(dat_sm$int_stat, levels = unique(dat_sm$int_stat))

  dat_sml <- dat_sm

  dat_sm <- data.table::dcast(as.data.table(dat_sm),
    cpu_date + subj + drug + dose + unit + measure ~ int_stat,
    value.var = "value"
  )

  # Split and long to wide again
  suppressWarnings(dat_fs <- split.data.frame(dat_sm, dat_sm$measure))

  # dat_sm is the datgrame used to make plots
  if (inter == TRUE) {
    svDialogs::dlg_message("Next select the folder where to save the summary", type = "ok")$res
    file_p <- svDialogs::dlg_dir()$res

    setwd(file_p)
    writexl::write_xlsx(dat_fs, "summary.xlsx")
  } else {
    return(list(dat_long = dat_long, dat_vent = dat_vent, dat_sml = dat_sml, dat_fs = dat_fs))
  }
}
