#' get the time of injection.
#'
#' inj_comments discovers the time of the injection of a dataframe returned by \code{\link{get_iox}}
#' by finding the position of comments (comments_tsd) with subject, drug, dose and units.  The function is used internally
#' by  \code{\link{import_session}}
#'
#' @param tsd dataframe of comments (2 columns).
#' @param detec regexp for detecting rows with multiple subjects
#' @param sep regexp  for separating subjects from rest
#' @return a dataframe tsd_s
#' @importFrom rlang .data
#' @export
split_comments <- function(tsd, detect, sep) {
  # vent <- dat
  # tsd = time_injection subject and drug
  # type of comments: ray1 heroin 600 ug/kg, rat 3 heroin 600 ug/kg, rats 9 and 11 - heroin 600 ug/kg
  # some comments indicate that at the same time 2 subjects have been injected. These comments have "AND"

  # tsd <- vent[vent$info %in% comments_tsd, c("timecpu_s", "info")]
  # tsd$info <- stringr::str_remove_all(tsd$info, "^([:alpha:]{3,4}\\s)|^[:alpha:]{3,4}") # rat rats or ray

  # this is a mess! In case of "rats 1 2 3 4 drug dose unit"
  toadd2 <- tsd[stringr::str_detect(tsd$info, "[0-9]\\s[0-9]+"), ] #get comments with "number number number"
  tsd <- dplyr::setdiff(tsd, toadd2) # remove them
  toadd2 <- tidyr::separate(toadd2, .data$info, sep = "(?<=[0-9])\\s(?=[A-z])", c("subj", "info"),
                           extra = "merge") # separate subject from rest: first space between number and word
  n_subj2 <- unlist(stringr::str_split(unlist(toadd2$subj), " ")) # create a column with subject
  col_subj2 <- n_subj2[n_subj2 != ""]
  rep_subj2 <- stringr::str_count(toadd2$subj, "[0-9]") # how many subjects in each comment?
  toadd2 <- toadd2[rep(seq_len(nrow(toadd2)), rep_subj2),] # repeat rows for each subject
  toadd2$subj <- col_subj2 # substitute subject

  # #  In case of "rats 1 and 2 grooming"
  # # we identify the comments, we duplicate the part after add and then we clean up at 135
  # toadd <- tsd[stringr::str_detect(tsd$info, "and"), ]
  # tsd <- dplyr::setdiff(tsd, toadd)
  # toadd <- tidyr::separate(toadd, .data$info, sep = "(?<=[0-9])\\s(?=[A-z])(?!and)", c("subj", "info"),
  #                           extra = "merge") # space between a number and a word that is not and
  # toadd$subj <- stringr::str_remove_all(toadd$subj, "and")
  # n_subj <- unlist(stringr::str_split(unlist(toadd$subj), " ")) # create a column with subject
  # col_subj <- n_subj[n_subj != ""]
  # rep_subj <- stringr::str_count(toadd$subj, "[0-9]")
  # toadd <- toadd[rep(seq_len(nrow(toadd)), rep_subj),]
  # toadd$subj <- col_subj
  #
  # tsd <- tidyr::separate(tsd, .data$info, sep = "(?<=[0-9])\\s", c("subj", "info"),
  #                          extra = "merge")
  #
  # tsd <- do.call(rbind, list(tsd, toadd, toadd2))
  #
  #
  # tsd$info <- stringr::str_remove_all(tsd$info, "(?!/)[:punct:]")
  #
  #
  # # now we split the comment, tsd_s = tsd separated
  # tsd_s<- tidyr::separate(tsd, .data$info, c("drug", "dose", "unit"), fill = "right", extra = "merge")
  #
  # rm_subj <- stringr::str_extract(unique(vent$subj_drug), "[0-9]+")
  # tsd_s <- tsd_s[tsd_s$subj %in%  rm_subj, ]
  #
  # return(tsd_s)
}
