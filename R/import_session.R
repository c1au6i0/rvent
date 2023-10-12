#' read iox txt files.
#'
#' get_iox reads and merges files created by the software SCIREQ. It returns a dataframe. The function is used internally by
#' \code{\link{import_session}}.
#'
#' @param iox_data path to folder containing data or, list of files to import when `shin_f = TRUE`.
#' @param inter logical for using or not dialogs to input data and select folders.
#' @param shiny_f TRUE if to be used in rvent_app.
#' @return a list of dataframe:
#' \itemize{
#'    \item vent: a list of dataframes of efach of the iox files in which time_s = 0 is the start of
#'    the session.
#'    \item tsd_s: dataframe of all comments with each row rappresenting a
#'    different subject.}
#' @seealso normalizetime_vent(), import_session(), split_comments().
#' @importFrom rlang .data
#' @export
get_iox <- function(iox_data, inter = TRUE, shiny_f = FALSE) {

  if (shiny_f == TRUE){
    if (missing(iox_data) || is.null(iox_data) ) stop("iox files missing!")
    files_imp  <- dplyr::filter(iox_data, stringr::str_detect(.data$name, pattern =  "iox.txt"))
    mess <- "You have not selected any iox.txt files!"
    if (length(files_imp) == 0) {
      stop(mess)
    }
    # not sure why it appears that files have different columns.
    # this is a workaround that extract name and drug form cell 16,7
    subj_info <- lapply(
      files_imp$datapath,
      vroom::vroom,
      skip = 15,
      n_max = 1,
      delim = "\t",
      col_names = FALSE,
      col_select = "X7",
      col_types = c(X7 = "c")
    )
  } else {
    mess <- "Choose folder containing iox.txt files of the session."
    if (inter == FALSE) {
      if (missing(iox_data)) stop("iox folder missing!")
    } else {
      svDialogs::dlg_message(mess, type = "ok")
      iox_data <- svDialogs::dlg_dir(title = mess)$res
    }

    list_files <- list.files(iox_data, full.names = TRUE)
    files_imp <- list_files[grepl(pattern = "*iox.txt", list_files)]

    mess <- "There are not iox.txt files in that folder!"
    if (length(files_imp) == 0) {
      if (inter == TRUE) {
        stop(svDialogs::dlgMessage(mess,
                                   type = "ok"
        )$res)
      } else {
        stop(mess)
      }
    }

    # not sure why it appears that files have different columns.
    # this is a workaround that extract name and drug form cell 16,7
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
  }

  subj_info2 <- lapply(subj_info, function(x) unlist(x)[[1]][1])
  subj <- as.numeric(stringr::str_extract(subj_info2, "[[:digit:]]+"))

  drug  <- gsub("^.*[0-9].*?([[:alpha:]]{3,}).*", "\\1", subj_info2)

  subj_drug_v <- paste0("rat", unlist(subj), "_", unlist(drug))

  if (shiny_f == TRUE) {
    vent <- vroom::vroom(files_imp$datapath,
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
  } else {
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
  }


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

  # vent$string_type <-
  vent$string_type[is.na(vent$string_type)] <-  0

  comments <- stats::na.omit(unique(vent$info[vent$string_type == c("comment")]))
  # eliminate comments without subjects (i.e. numbers)
  comments <- comments[stringr::str_detect(comments, "[1-9]")]

  # tsd = time_injection subject and drug
  tsd <- vent[vent$info %in% comments, c("cpu_date", "timecpu_s", "info")]
  tsd$info <- stringr::str_remove_all(tsd$info, "^([:alpha:]{3,4}\\s)|^[:alpha:]{3,4}") # rat rats or ray

  # "number number number"
  tsd <- split_comments(tsd, detect = "[0-9]\\s[0-9]+", sep = "(?<=[0-9])\\s(?=[A-z])", rem = "")
  tsd$subj <- as.numeric(tsd$subj)

  tsd <- tidyr::unite(tsd, "info", .data$subj, .data$info, sep = " ")
  # space between a number and a word that is not "and"
  tsd <- split_comments(tsd, detect = "and", sep = "(?<=[0-9])\\s(?=[A-z])(?!and)", rem = "and")

  tsd$info <- stringr::str_remove_all(tsd$info, "(?!/)[:punct:]")

  # now we split the comment, tsd_s = tsd separated
  tsd_s <- tidyr::separate(tsd, .data$info, c("drug", "dose", "unit"), fill = "right", extra = "merge")


  rm_subj <- stringr::str_extract(unique(vent$subj_drug), "[0-9]+")
  tsd_s <- tsd_s[tsd_s$subj %in% rm_subj, ]

  return(list(vent = vent, tsd_s = tsd_s))
}

#' normalize time of injection.
#'
#' normalizetime_vent tidies up the ataframes returned by \code{\link{get_iox}} and creates a column of time from injection in seconds.
#' The function is used internally by \code{\link{import_session}}.
#'
#' @param dat a dataframe returned by \code{\link{get_iox}}.
#' @param tsd_s the dataframe of comments returned by \code{\link{get_iox}}.
#' @param tofill vector of values to replace missing data in tsd_s (used if inter = FALSE).
#' @param baseline length of baseline in minutes.
#' @seealso get_iox(), import_session(), split_comments().
#' @importFrom rlang .data
#' @export
normalizetime_vent <- function(dat, tsd_s, tofill, baseline = 30) {

  vent <- dat
  tsd_s <- as.data.frame(tsd_s)
  vent <- tidyr::separate(vent, .data$subj_drug, c("subj", "drug"), remove = TRUE)
  if (!is.null(tofill)) tsd_s[is.na(tsd_s)] <- tofill
  names(tsd_s)[names(tsd_s) == "timecpu_s"] <- "time_inj"
  tsd_s[, "subj"] <- as.factor(paste0("rat", tsd_s$subj))


  vent$cpu_date <- as.character(vent$cpu_date)

  # join tsd_s and vent to add column with injection time.
  suppressWarnings(vent_j <- dplyr::inner_join(vent, tsd_s, by = c("subj", "cpu_date")))

  # split
  vent_jn <- split.data.frame(vent_j, list(as.factor(vent_j$subj), as.factor(vent_j$cpu_date)), drop =
                                 TRUE)

  # normalize: 0 injection
  baseline <- baseline * 60 # 30 min baseline
  vent_jn <- lapply(vent_jn, function(x) {
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
#' Imports txt files created by the software SCIREQ and return a list of dataframes.
#'
#' @param baseline length of baseline in minutes.
#' @param inter logical for using or not dialogs to input data and select folders.
#' @param iox_data path to folder containing data or, list of files to import when `shin_f = TRUE`.
#' @param comments_tsd vector of comments that contain doses (used if inter = FALSE).
#' @param tofill vector of values to replace missing data in comments_tsd (used if inter = FALSE).
#' @seealso normalizetime_vent(), import_session(), split_comments().
#' @importFrom rlang .data
#' @export
import_session <- function(iox_data, baseline = 30, inter = TRUE, comments_tsd, tofill = NULL) {
  if (inter == TRUE) {
    all_data <- get_iox()
  } else {
    all_data <- get_iox(iox_data = iox_data, inter = FALSE)
  }

  vent <- all_data$vent
  choose_comments <- all_data$tsd_s
  #----------------------------------------------#

  choose_comments <- tidyr::unite(choose_comments, col = "subj_drug_dose_unit", .data$subj, .data$drug, .data$dose, .data$unit, sep = " ")

  if (inter == FALSE) {
    if (missing(comments_tsd)) stop("comments_tsd missing!")
    if (length(comments_tsd) == 0) stop("no injection time!")
  } else {
    comments_tsd <- svDialogs::dlg_list(choose_comments$subj_drug_dose_unit, multiple = TRUE, title = "Choose the comments containing subject and drug administered")$res
  }

  tsd_s <- choose_comments[choose_comments$subj_drug_dose_unit %in% comments_tsd, ]
  tsd_s <- tidyr::separate(tsd_s, .data$subj_drug_dose_unit, c("subj", "drug", "dose", "unit"), fill = "right", extra = "merge")
  tsd_s$cpu_date <- as.character(tsd_s$cpu_date)
  tsd_s[tsd_s == "NA"] <- NA
  na_pos <- dplyr::arrange(as.data.frame(which(is.na(tsd_s), arr.ind = TRUE)), row)

  if (inter == TRUE) {
    if (nrow(na_pos) > 0) {
      for (x in as.numeric(unique(na_pos$row))) {
        mesg <- paste(list("subj =", "; drug =", "; dose =", "; unit ="), unlist(tsd_s[x, 3:ncol(tsd_s)]), collapse = " ")
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
