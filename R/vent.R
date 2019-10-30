#' Class vent
#'
#' Method, constructor and validator of objects of class vent
#'
#' @param  dat an object of class dataframe
#' @param ... dots
#' @return  an object of class vent
#' @aliases autoplot.vent, new_vent, validate_vent, as.data.frame.vent
#' @export vent
"vent" <- function(dat, ...) UseMethod("vent")


#' vent constructor
#'
#' @concept \url{https://adv-r.hadley.nz/s3.html#s3-constructor}{advance R}
#' @param dat object
#' @export new_vent
new_vent <- function(dat = data.frame()) {
  stopifnot(is.data.frame(dat))
  structure(dat, class = "vent", bin = unique(dat$bin), baseline = unique(dat$baseline))
}

#' #' vent validate
#' #'
#' #' validate an object of class vent
#' #'
#' #' @param dat object
#' #' @export validate_vent
#' validate_vent <- function(dat){
#'
#'   obtained <- as.data.frame(sapply(dat, typeof))
#'
#'   if(!all(expect = names(dat))) stop("Wrong columns: check number of columns and or/and their names")
#'
#'
#'   values <- unclass(dat)
#'   names(attributes(dat))
#' }

#' dat_vent
#'
#' an object of class vetn
