#' Import iox session.
#'
#' Import txt files created by the software IOX and return a list of dataframes of class iox.
#'
#' @param baseline Length of baseline in minutes.
#' @param inter logical for using or  not dialogs to input data and select folders.
#' @param iox_folder path to folder for saving data, used if inter = FALSE.
#' @param comments_tsd vector of comments that contain doses, used if inter = FALSE.
#' @param tofill vector of values to replace missing data in comments_tsd, used if inter = FALSE.
#' @return A list of dataframes of class iox.
#' @importFrom rlang .data
#' @export
import_session <- function(baseline = 30, inter = TRUE, iox_folder, comments_tsd, tofill){

  if (inter == FALSE){
    if (missing(iox_folder)) stop("iox_folder missing!")
  } else {
  svDialogs::dlg_message("Choose folder containing  iox.txt files of the session.", type = "ok")
  iox_folder <- svDialogs::dlg_dir(title = "Choose folder containing  iox.txt files of the session.")$res
  }

  list_files <- list.files(iox_folder, full.names = TRUE)
  files_imp <- list_files[grepl(pattern = "*iox.txt", list_files)]

  if(length(files_imp) == 0) stop(svDialogs::dlgMessage("There are not iox files
                                                          Make sure to have files that end with iox.txt", type = "ok")$res)

  # not sure why it appears that files have different columns.
  # this is a workaround that
  # extract name and drug form cell 16,7
  subj_info  <- lapply(files_imp, vroom::vroom,
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

  subj_drug_v <- paste0("rat", unlist(subj), "_",  unlist(drug))

  vent <- vroom::vroom(files_imp,
                delim ="\t",
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
                                              orders = c("%b %d, %Y",  "%d-%b-%y"))

  vent <- tidyr::separate(vent, .data$cpu_time, c("cpu_time", "time_ms"), sep ="\\.")
  vent$cpu_time <- as.POSIXct(vent$cpu_time, format = "%I:%M:%S %p")
  vent[, "timecpu_s"] <- as.numeric(vent$time_ms)/1000 +
            as.numeric(format(vent$cpu_time, '%S')) +
            as.numeric(format(vent$cpu_time, '%M')) *60 +
            as.numeric(format(vent$cpu_time, '%I')) *3600

  # recode use recode_col instead: recode_col(vent$id, subj_drug_v)
  vent_id <- as.character(unique(vent$id))
  id_recode <- subj_drug_v[seq_along(vent_id)]
  newnames <- stats::setNames(id_recode, vent_id)
  vent[,"subj_drug"] <- newnames[vent$id]
  vent$subj_drug <-as.factor(newnames[vent$id])

  vent$string_type <- tidyr::replace_na(vent$string_type, 0)

  # comments
  comments <- unique(vent$info[vent$string_type == c("comment")])

  # tsd = time_injection subject and drug

  if (inter == FALSE){
    if (missing(comments_tsd)) stop("comments_tsd missing!")
    if (length(comments_tsd) == 0) stop("no injection time!")
  } else {
    comments_tsd <- svDialogs::dlg_list(stats::na.omit(comments), multiple = TRUE, title = "Choose the comments containing subject and drug administered")$res
  }

  # tsd = time_injection subject and drug
  # type of comments: ray1 heroin 600 ug/kg, rat 3 heroin 600 ug/kg, rats 9 and 11 - heroin 600 ug/kg
  # some comments indicate that at the same time 2 subjects have been injected. These comments have "AND"

  tsd <- vent[vent$info %in% comments_tsd, c("timecpu_s","info")]

  # if there are 2 subjects in the same comment
  toadd <- tsd[stringr::str_detect(tsd$info, "and"),]
  toadd$info <- stringr::str_extract(toadd$info, "(?<=and ).*")
  tsd <- rbind(tsd, toadd)

  # TRY ALL IN ONE CALL
  tsd$info <- stringr::str_remove_all(tsd$info, "(\\sand\\s[0-9]+\\s)")
  tsd$info <- stringr::str_remove_all(tsd$info, "^([:alpha:]{3,4}\\s)|^[:alpha:]{3,4}") # rat rats or ray
  tsd$info <- stringr::str_remove_all(tsd$info, "(?!/)[:punct:]")

  # now we split them, tsd_s = tsd separated
  tsd_s <- tidyr::separate(tsd, .data$info, c("subj", "drug", "dose", "unit"), fill = "right", extra = "merge")

  # Replace NA positions
  na_pos <- dplyr::arrange(as.data.frame(which(is.na(tsd_s), arr.ind = TRUE)), row)

  if(inter == FALSE){
    if (missing(tofill)) stop("tofill missing!")
    tsd_s[is.na(tsd_s)] <- tofill
    }else{
      if (nrow(na_pos) > 0) {
        for (x in as.numeric(unique(na_pos$row))) {
          mesg <-  paste(list("subj =","; drug =", "; dose =", "; unit ="), unlist(tsd_s[x, 2:ncol(tsd_s)]), collapse = " ")
          tofill <- svDialogs::dlg_input(paste0("Enter any missing values (NA) in:  ", mesg))$res
          tofill <- unlist(strsplit(tofill, " "))
          prova <- is.na(tsd_s[x,])
          tsd_s[x, as.vector(is.na(tsd_s[x,]))] <- tofill}
      }
    }

  names(tsd_s)[names(tsd_s) == "timecpu_s"] <- "time_inj"
  tsd_s[, "subj_drug"] <- as.factor(paste0("rat", tsd_s$subj, "_", tsd_s$drug))


  # vent_joined
  suppressWarnings(vent_j <- dplyr::inner_join(vent, tsd_s, by = c("subj_drug")))

  # split
  vent_jn <- split.data.frame(vent_j, as.factor(vent_j$subj_drug))

  # normalize: 0 injection
  baseline <- 30 * 60 # 30 min baseline
  vent_jn <- lapply(vent_jn, function(x){
      # start_time <- min(x[["timecpu_s"]], na.rm = TRUE)
      # x[, "time_s"] <- x[["time_s"]] - start_time
      # x[, "time_inj"] <- as.numeric(x[["time_inj"]]) - start_time
      # x[, "start_time"] <- start_time
      x[, "time_s"] <- x[["timecpu_s"]] - x[["time_inj"]]
      x <-   x[x$time_s >= -baseline, ]
      class(x) <- c(unlist(class(x)), "iox")
      return(x)
  })
  class(vent_jn) <- c(unlist(class(vent_jn)), "iox")
  vent_jn
}





