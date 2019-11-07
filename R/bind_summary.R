#' bind_summary
#'
#' bind together xlsx files returned by \code{\link{summarize_vent}}
#'
#' @param inter logical.
#' @param path if inter is false, path of xlsx.
#' @return an xlsx file or a dataframe
#' @importFrom rlang .data
#' @import stats
#' @export
bind_summary <- function(inter = TRUE, path) {
  if (inter == FALSE) {
    if (missing(path)) stop("path missing!")
    xlsx_folder <- path
  } else {
    svDialogs::dlg_message("Choose folder containing summary files of the session.", type = "ok")
    xlsx_folder <- svDialogs::dlg_dir(title = "Choose folder containing  iox.txt files of the session.")$res
  }

  list_files <- list.files(xlsx_folder, full.names = TRUE)
  files_imp <- list_files[grepl(pattern = "*.xlsx", list_files)]

  if (length(files_imp) == 0) stop(svDialogs::dlgMessage("There are not xlsx files
                                                          Make sure to have files that end with .xlsx", type = "ok")$res)

  # name the dataframe

  dat_all <- purrr::map_dfr(files_imp, rio::import_list, rbind = TRUE, rbind_label = "label")
  dat_all <- dat_all[, names(dat_all) != "label"]

  dates <- paste(as.character(unique(dat_all$cpu_date)), collapse = "_")
  suppressWarnings(dat_fs <- split.data.frame(dat_all, dat_all$measure))

  if (inter == TRUE) {
    svDialogs::dlg_message("Next select the folder where to save the summary", type = "ok")$res
    file_p <- svDialogs::dlg_dir()$res
    setwd(file_p)
    writexl::write_xlsx(dat_fs, paste0("summary_", dates, ".xlsx"))
  } else {
    return(dat_fs)
  }
}
