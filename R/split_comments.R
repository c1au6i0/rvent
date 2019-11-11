#' split comments.
#'
#' split_comments reorganizes a dataframe with column "time" and "comments" to return a dataframe in which
#' each subj is in a different row. It is used internally by \code{\link{get_iox}}. Patterns are "rat 1 and 2 and 3 ..." and
#' "rat 1 2 3.."
#'
#' @param tsd dataframe of comments (2 columns).
#' @param detec regexp for detecting rows with multiple subjects
#' @param sep regexp  for separating subjects from rest
#' @param rem the string to remove
#' @return a dataframe tsd_s
#' @importFrom rlang .data
#' @export
split_comments <- function(tsd, detect, sep, rem) {
  toadd <- tsd[stringr::str_detect(tsd$info, detect), ] # get comments with "number number number"
  tsd2 <- dplyr::setdiff(tsd, toadd) # remove them
  toadd <- tidyr::separate(toadd, .data$info,
    sep = sep, c("subj", "info"),
    extra = "merge"
  ) # separate subject from rest: first space between number and word
  n_subj <- unlist(stringr::str_split(unlist(toadd$subj), " ")) # create a column with subject
  col_subj <- n_subj[n_subj != rem]
  rep_subj <- stringr::str_count(toadd$subj, "[0-9]") # how many subjects in each comment?
  toadd <- toadd[rep(seq_len(nrow(toadd)), rep_subj), ] # repeat rows for each subject
  toadd$subj <- col_subj # substitute subject
  tsd2 <- tidyr::separate(tsd, .data$info,
    sep = "(?<=[0-9])\\s", c("subj", "info"),
    extra = "merge"
  )

  tsd2 <- rbind(tsd2, toadd)

  return(tsd2)
}