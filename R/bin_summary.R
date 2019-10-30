#' find_bins in vent dataframes.
#'
#' find_bins creates left closed duration intervals and filters a vent dataframe.
#'
#' @param dat vent dataframe.
#' @param baseline baseline duration in minutes
#' @param bin bin duration in minutes
#' @return a vent dataframe with a column "int_min" indicating the left limit of the interval in minutes and, a
#'    a colum "int_sec" indicating the left and right limits of the interval in seconds. Time of injection is at time 0.
#' @importFrom rlang .data
#' @note this function is used by s3ummarize_vent
#' @export
find_bins <- function(dat, baseline, bin) {

   # filter based on some acceptance criteria in rats
   dat <- dat[
         dat$TV_ml >= 0.04 &
         dat$TV_ml <= 10 &

         dat$f_bpm >= 10 &
         dat$f_bpm <= 250, ]

   dat <- dat[!is.na(dat$time_s), ]

   # is bin a multiple of the length of th session?
   # first and last full interval...rounded at a minute (so it can be up to 24 sec off)
   min_m <- round(min(dat$time_s)/60/bin) * bin
   max_m <- round(max(dat$time_s)/60/bin) * bin

   dat <-  dat[dat$time_s >= min_m * 60 &
                  dat$time_s <= max_m * 60, ]

   # interval
   intv  <- seq(min_m * 60, max_m * 60, by = bin * 60)


   dat$intv <- findInterval(dat$time_s, intv)

   # recode
   dat[,"int_min"] <- recode_col(dat$intv, intv)/60
   dat[, "int_sec"] <- cut(dat$time_s, intv)
   return(dat)
}


