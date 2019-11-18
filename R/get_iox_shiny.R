#' read iox txt files.
#'
#' get_iox reads and merges files created by the software SCIREQ. It returns dataframes. The function is used internally by
#' \code{\link{import_session}}.
#'
#' @param iox_files input of shiny::fileInput.
#' @return a list of dataframe:
#' \itemize{
#'    \item vent: a list of dataframes of each of the iox files in which time_s = 0 is the start of
#'    the session.
#'    \item tsd_s: dataframe of all comments with each row rappresenting a
#'    different subject.}
#' @seealso normalizetime_vent(), import_session(), split_comments().
#' @importFrom rlang .data
#' @export
get_iox_shiny <- function(iox_files) {

  if (missing(iox_files) || is.null(iox_files) ) stop("iox_files missing!")


  files_imp  <- dplyr::filter(iox_files, stringr::str_detect(.data$name, pattern =  "iox.txt"))


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

  subj_info2 <- lapply(subj_info, function(x) unlist(x)[[1]][1])
  subj <- stringr::str_extract(subj_info2, "[[:digit:]]+")
  drug <- stringr::str_extract(subj_info2, "[[:alpha:]]{4,}")

  if (anyDuplicated(subj) != 0) {
    stop("There 2 or more sessions of the same subject!")
  }

  subj_drug_v <- paste0("rat", unlist(subj), "_", unlist(drug))

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

  comments <- stats::na.omit(unique(vent$info[vent$string_type == c("comment")]))
  # eliminate comments without subjects (i.e. numbers)
  comments <- comments[stringr::str_detect(comments, "[1-9]")]

  # tsd = time_injection subject and drug
  tsd <- vent[vent$info %in% comments, c("timecpu_s", "info")]
  tsd$info <- stringr::str_remove_all(tsd$info, "^([:alpha:]{3,4}\\s)|^[:alpha:]{3,4}") # rat rats or ray

  # "number number number"
  tsd <- split_comments(tsd, detect = "[0-9]\\s[0-9]+", sep = "(?<=[0-9])\\s(?=[A-z])", rem = "")
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
