#' Import data ventilation.
#'
#' Import a txt file created by the software IOX and return a dataframe of vent.
#'
#' @param link Path of the file to import.
#' @return A dataframe of class fightscore. Columns are:
#' \describe{
#'   \item{xx}{xxxx}
#' }

#' @importFrom dplyr filter mutate rename select
#' @importFrom tidyr separate
#' @importFrom rlang .data
#' @seealso  \url{https://www.boris.unito.it/}
#' @export

iox_folder <- svDialogs ::dlgDir()$res
list_files <- list.files(iox_folder, full.names = TRUE)
files_imp <- list_files[grepl(pattern = "*iox.txt", list_files)]
#
# if(length(files_imp) == 0) stop(svDialogs::dlgMessage("There are not iox files
#                                                         Make sure to have files that end with iox.txt", type = "ok")$res)




import_iox <- function(){

  # link_file <- svDialogs::dlgOpen()$res
  iox_folder <- svDialogs ::dlgDir()$res
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

  names(vent) <- unlist(vent_head)

  #time in seconds from cpu_time, unfortunately the other columns reset after 1h to 0
  vent$cpu_date <- as.Date(vent$cpu_date, "%d-%b-%y")
  vent <- tidyr::separate(vent, cpu_time, c("cpu_time", "time_ms"), sep ="\\.")
  vent$cpu_time <- as.POSIXct(vent$cpu_time, format = "%H:%M:%S %p")
  vent[, "time_s"] <- as.numeric(vent$time_ms)/1000 +
            as.numeric(format(vent$cpu_time, '%S')) +
            as.numeric(format(vent$cpu_time, '%M')) *60 +
            as.numeric(format(vent$cpu_time, '%H')) *3600

  # recode
  vent_id <- as.character(unique(vent$id))
  id_recode <- subj_drug[seq_along(vent_id)]
  newnames <- setNames(id_recode, vent_id)
  vent[,"subj_drug"] <- newnames[vent$id]
  vent$subj_drug <-as.factor(newnames[vent$id])

  vent$string_type <- tidyr::replace_na(vent$string_type, 0)

  # comments
  comments <- unique(vent$info[vent$string_type == c("comment")])

  # tsd = time_injection subject and drug
  comments_tsd <- svDialogs::dlg_list(na.omit(comments), multiple = TRUE, title = "Choose the comments containing subject and drug administered")$res

  # tsd = time_injection subject and drug
  # type of comments: ray1 heroin 600 ug/kg, rat 3 heroin 600 ug/kg, rats 9 and 11 - heroin 600 ug/kg
  # some comments indicate that at the same time 2 subjects have been injected. These comments have "AND"

  tsd <- vent[vent$info %in% comments_tsd, c("time_s","info")]

  # if there are 2 subjects in the same comment
  toadd <- tsd[stringr::str_detect(tsd$info, "and"),]
  toadd$info <- stringr::str_extract(toadd$info, "(?<=and ).*")
  tsd <- rbind(tsd, toadd)

  # TRY ALL IN ONE
  tsd$info <- stringr::str_remove_all(tsd$info, "(\\sand\\s[0-9]+\\s)")
  tsd$info <- stringr::str_remove_all(tsd$info, "^([:alpha:]{3,4}\\s)|^[:alpha:]{3,4}") # rat rats or ray
  tsd$info <- stringr::str_remove_all(tsd$info, "(?!/)[:punct:]")

  # now we split them, tsd_s = tsd separated
  tsd_s <- tidyr::separate(tsd, info, c("subj", "drug", "dose", "unit"), fill = "right", extra = "merge")

  # Replace NA positions
  na_pos <- dplyr::arrange(as.data.frame(which(is.na(tsd_s), arr.ind = TRUE)), row)

  if (nrow(na_pos) > 0) {
    for (x in as.numeric(unique(na_pos$row))) {
      mesg <-  paste(list("subj =","; drug =", "; dose =", "; unit ="), unlist(tsd_s[x, 2:ncol(tsd_s)]), collapse = " ")
      tofill <- svDialogs::dlg_input(paste0("Enter missing values (NA) separated by space of ", mesg))$res
      tofill <- unlist(strsplit(tofill, " "))
      prova <- is.na(tsd_s[x,])
      tsd_s[x, as.vector(is.na(tsd_s[x,]))] <- tofill
    }
  }

  names(tsd_s)[names(tsd_s) == "time_s"] <- "time_inj"

  # remember to normalize the time!

  # vent_joined
  vent_j <- dplyr::inner_join(vent, tsd_s, by = c("subj", "drug"),)

  vent_j[, "subj_drug"] <- paste(vent_j$subj, vent_j$drug, sep = "_")

  # split
  vent_jn <- split.data.frame(vent_j, as.factor(vent_j$subj_drug))

  # normalized
  vent_jn <- lapply(vent_jn, function(x){
      minimum <- min(x[["time_s"]], na.rm = TRUE)
      x[, "time_s"] <- x[["time_s"]] - minimum
      x[, "time_inj"] <- as.numeric(x[["time_inj"]]) - minimum
      x[, "minimum"] <- minimum
      return(x)
  })

  # create intervals based on injection


}


