#' read iox txt files.
#'
#' get_iox reads and merges files created by the software SCIREQ. It return a dataframe. The function is used internally by
#' \code{\link{import_session}}
#'
#' @param iox_folder path to folder for saving data, used if inter = FALSE.
#' @param baseline Length of baseline in minutes.
#' @param inter logical for using or not dialogs to input data and select folders.
#' @return a dataframes.
#' @seealso
#' * [inj_comments()].
#' * [normalizetime_vent()].
#' * [import_session()].
#' @importFrom rlang .data
#' @export
get_iox <- function(iox_folder, baseline = 30, inter = TRUE) {
  list_files <- list.files(iox_folder, full.names = TRUE)
  files_imp <- list_files[grepl(pattern = "*iox.txt", list_files)]

  if (length(files_imp) == 0) {
    if (inter == TRUE ) {
    stop(svDialogs::dlgMessage("There are not iox files Make sure to have files that end with iox.txt",
        type = "ok")$res)
    } else {
      stop("There are not iox files Make sure to have files that end with iox.txt")
    }
  }

  # not sure why it appears that files have different columns.
  # this is a workaround that
  # extract name and drug form cell 16,7
  subj_info <- lapply(
    files_imp,
    vroom::vroom,
    skip = 15,
    n_max = 1,
    delim = "\t",
    col_names = FALSE,
    col_select = "X7",
    col_types = c(X7 = "c")
  )

  subj_info2 <- lapply(subj_info, function(x) unlist(x)[[1]][1])
  subj <- stringr::str_extract(subj_info2, "[[:digit:]]+")
  drug <- stringr::str_extract(subj_info2, "[[:alpha:]]{4,}")

  subj_drug_v <- paste0("rat", unlist(subj), "_", unlist(drug))

  vent <- vroom::vroom(files_imp,
    delim = "\t",
    skip = 41,
    col_names = FALSE,
    col_types = list(
      X6 = "c",
      X7 = "c",
      X8 = "c",
      X9 = "c"
    ),
    id = "id"
  )

  names(vent) <- unlist(iox_head)

  # time in seconds from cpu_time, unfortunately the other columns reset to 0 after 1h
  # Aug 19, 2019, 19-Mar-19


  vent$cpu_date <- lubridate::parse_date_time(vent$cpu_date,
    orders = c("%b %d, %Y", "%d-%b-%y")
  )

  vent <- tidyr::separate(vent, .data$cpu_time, c("cpu_time", "cpu_ms"), sep = "\\.")
  vent$cpu_time <- as.POSIXct(vent$cpu_time, format = "%I:%M:%S %p")
  vent[, "timecpu_s"] <- as.numeric(vent$cpu_ms) / 1000 +
    as.numeric(format(vent$cpu_time, "%S")) +
    as.numeric(format(vent$cpu_time, "%M")) * 60 +
    as.numeric(format(vent$cpu_time, "%H")) * 3600

  # recode \
  vent_id <- as.character(unique(vent$id))
  id_recode <- subj_drug_v[seq_along(vent_id)]
  newnames <- stats::setNames(id_recode, vent_id)
  vent[, "subj_drug"] <- newnames[vent$id]
  vent$subj_drug <- as.factor(newnames[vent$id])

  vent$string_type <- tidyr::replace_na(vent$string_type, 0)

  # comments
  # comments <- unique(vent$info[vent$string_type == c("comment")])

  return(vent)
}

#' get the time of injection.
#'
#' inj_comments discovers the time of the injection of a dataframe returned by \code{\link{get_iox}}
#' by finding the position of comments (comments_tsd) with subject, drug, dose and units.  The function is used internally
#' by  \code{\link{import_session}}
#'
#' @param dat a dataframe returned by \code{\link{get_iox}}.
#' @return a dataframe tsd_s
#' @seealso
#' * [get_iox()].
#' * [normalizetime_vent()].
#' * [import_session()].
#' @importFrom rlang .data
#' @export
inj_comments <- function(dat) {

  vent <- dat

  comments <- stats::na.omit(unique(vent$info[vent$string_type == c("comment")]))

  # eliminate comments without subjects (i.e. numbers)
  comments <- comments[stringr::str_detect(comments, "[1-9]")]

  # tsd = time_injection subject and drug
  # type of comments: ray1 heroin 600 ug/kg, rat 3 heroin 600 ug/kg, rats 9 and 11 - heroin 600 ug/kg
  # some comments indicate that at the same time 2 subjects have been injected. These comments have "AND"
  # other have multiple numbers with multiple subjects

  tsd <- vent[vent$info %in% comments, c("timecpu_s", "info")]
  tsd$info <- stringr::str_remove_all(tsd$info, "^([:alpha:]{3,4}\\s)|^[:alpha:]{3,4}") # rat rats or ray

  # this is a mess! In case of "rats 1 2 3 4 drug dose unit"
  toadd2 <- tsd[stringr::str_detect(tsd$info, "[0-9]\\s[0-9]+"), ] #get comments with "number number number"
  tsd <- dplyr::setdiff(tsd, toadd2) # remove them
  toadd2 <- tidyr::separate(toadd2, .data$info, sep = "(?<=[0-9])\\s(?=[A-z])", c("subj", "info"),
                           extra = "merge") # separate subject from rest
  n_subj2 <- unlist(stringr::str_split(unlist(toadd2$subj), " ")) # create a column with subject
  col_subj2 <- n_subj2[n_subj2 != ""]
  rep_subj2 <- stringr::str_count(toadd2$subj, "[0-9]") # how many subjects in each comment?
  toadd2 <- toadd2[rep(seq_len(nrow(toadd2)), rep_subj2),] # repeat rows for each subject
  toadd2$subj <- col_subj2 # substitute subject

  #  In case of "rats 1 and 2 grooming"
  # we identify the comments, we duplicate the part after add and then we clean up at 135
  toadd <- tsd[stringr::str_detect(tsd$info, "and"), ]
  tsd <- dplyr::setdiff(tsd, toadd)
  toadd <- tidyr::separate(toadd, .data$info, sep = "(?<=[0-9])\\s(?=[A-z])(?!and)", c("subj", "info"),
                            extra = "merge") # space between a number and a word that is not and
  toadd$subj <- stringr::str_remove_all(toadd$subj, "and")
  n_subj <- unlist(stringr::str_split(unlist(toadd$subj), " ")) # create a column with subject
  col_subj <- n_subj[n_subj != ""]
  rep_subj <- stringr::str_count(toadd$subj, "[0-9]")
  toadd <- toadd[rep(seq_len(nrow(toadd)), rep_subj),]
  toadd$subj <- col_subj

  tsd <- tidyr::separate(tsd, .data$info, sep = "(?<=[0-9])\\s", c("subj", "info"),
                           extra = "merge")

  tsd <- do.call(rbind, list(tsd, toadd, toadd2))


  tsd$info <- stringr::str_remove_all(tsd$info, "(?!/)[:punct:]")


  # now we split the comment, tsd_s = tsd separated
  tsd_s <- tidyr::separate(tsd, .data$info, c("drug", "dose", "unit"), fill = "right", extra = "merge")

  rm_subj <- stringr::str_extract(unique(vent$subj_drug), "[0-9]+")
  tsd_s <- tsd_s[tsd_s$subj %in%  rm_subj, ]

  return(tsd_s)
}

#' normalize time of injection.
#'
#' normalizetime_vent tidies up a dataframe returned by \code{\link{get_iox}} and creates a column of time from injection in seconds
#' The function is used internally by \code{\link{import_session}}
#'
#' @param dat a dataframe returned by \code{\link{get_iox}}.
#' @param tsd_s the dataframe returned by \code{\link{inj_comments}}.
#' @param tofill vector of values to replace missing data in comments_tsd, used if inter = FALSE.
#' @param baseline Length of baseline in minutes.
#' @return a dataframe.
#' @seealso
#' * [get_iox()].
#' * [inj_comments()].
#' * [import_session()].
#' @importFrom rlang .data
#' @export
normalizetime_vent <- function(dat, tsd_s, tofill, baseline) {
  vent <- dat
  if (!is.null(tofill)) tsd_s[is.na(tsd_s)] <- tofill
  names(tsd_s)[names(tsd_s) == "timecpu_s"] <- "time_inj"
  tsd_s[, "subj_drug"] <- as.factor(paste0("rat", tsd_s$subj, "_", tsd_s$drug))


  # Join tsd_s and vent to add column with injection time.
  suppressWarnings(vent_j <- dplyr::inner_join(vent, tsd_s, by = c("subj_drug")))

  # split
  vent_jn <- split.data.frame(vent_j, as.factor(vent_j$subj_drug))

  # normalize: 0 injection
  baseline <- baseline * 60 # 30 min baseline
  vent_jn <- lapply(vent_jn, function(x) {
    # start_time <- min(x[["timecpu_s"]], na.rm = TRUE)
    # x[, "time_s"] <- x[["time_s"]] - start_time
    # x[, "time_inj"] <- as.numeric(x[["time_inj"]]) - start_time
    # x[, "start_time"] <- start_time
    x[, "time_s"] <- x[["timecpu_s"]] - x[["time_inj"]]
    x <- x[x$time_s >= -baseline, ]
    class(x) <- c(unlist(class(x)), "iox")
    return(x)
  })
  class(vent_jn) <- c(unlist(class(vent_jn)), "iox")
  vent_jn
}

#' Import iox session.
#'
#' Import txt files created by the software SCIREQ and return a list of dataframes of class iox.
#'
#' @param baseline Length of baseline in minutes.
#' @param inter logical for using or not dialogs to input data and select folders.`
#' @param iox_folder path to folder for saving data, used if inter = FALSE.
#' @param comments_tsd vector of comments that contain doses, used if inter = FALSE.
#' @param tofill vector of values to replace missing data in comments_tsd, used if inter = FALSE.
#' @return A list of dataframes of class iox.
#' @seealso
#' * [get_iox()].
#' * [inj_comments()].
#' * [normalizetime_vent()].
#' @importFrom rlang .data
#' @export
import_session <- function(iox_folder, baseline = 30, inter = TRUE, comments_tsd, tofill = NULL) {
  if (inter == FALSE) {
    if (missing(iox_folder)) stop("iox_folder missing!")
  } else {
    svDialogs::dlg_message("Choose folder containing  iox.txt files of the session.", type = "ok")
    iox_folder <- svDialogs::dlg_dir(title = "Choose folder containing  iox.txt files of the session.")$res
  }

  #----------------------------------------------#
  vent <- get_iox(iox_folder, baseline = baseline)
  #----------------------------------------------#

  # All this is to identify where the injection was given based on comments.
  # tsd = time_injection subject and drug

  #----------------------------------------------#
  choose_comments <- inj_comments(dat = vent)
  #----------------------------------------------#

  choose_comments <- tidyr::unite(choose_comments, col = "subj_drug_dose_unit", .data$subj, .data$drug, .data$dose,.data$unit, sep= " " )

  if (inter == FALSE) {
    if (missing(comments_tsd)) stop("comments_tsd missing!")
    if (length(comments_tsd) == 0) stop("no injection time!")
  } else {
    comments_tsd <- svDialogs::dlg_list(choose_comments$subj_drug_dose_unit, multiple = TRUE, title = "Choose the comments containing subject and drug administered")$res
  }

  tsd_s <- choose_comments[choose_comments$subj_drug_dose_unit %in% comments_tsd,]

  tsd_s <- tidyr::separate(tsd_s, .data$subj_drug_dose_unit, c("subj", "drug", "dose", "unit"), fill = "right", extra = "merge")

  tsd_s[tsd_s == "NA"] <- NA

  na_pos <- dplyr::arrange(as.data.frame(which(is.na(tsd_s), arr.ind = TRUE)), row)
  if (inter == TRUE) {
    if (nrow(na_pos) > 0) {
      for (x in as.numeric(unique(na_pos$row))) {
        mesg <- paste(list("subj =", "; drug =", "; dose =", "; unit ="), unlist(tsd_s[x, 2:ncol(tsd_s)]), collapse = " ")
        tofill <- svDialogs::dlg_input(paste0("Enter any missing values (NA) in: ", mesg))$res
        tofill <- unlist(strsplit(tofill, " "))
        prova <- is.na(tsd_s[x, ])
        tsd_s[x, as.vector(is.na(tsd_s[x, ]))] <- tofill
      }
    } else {
      tofill <- NULL
    }
  }

  vent_jn <- normalizetime_vent(dat = vent, tsd_s = tsd_s, tofill = tofill, baseline = baseline)

  return(vent_jn)
}
