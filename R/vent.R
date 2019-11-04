#' Class vent
#'
#' Method, constructor and validator of objects of class vent
#'
#' @param  dat an object of class dataframe
#' @param ... dots
#' @return  an object of class vent
#' @seealso  autoplot.vent, new_vent, validate_vent
#' @export vent
"vent" <- function(dat, ...) NextMethod("vent")


#' vent constructor
#'
#' @concept \url{https://adv-r.hadley.nz/s3.html#s3-constructor}{advance R}
#' @param dat object
#' @export new_vent
new_vent <- function(dat = data.frame()) {
  stopifnot(is.data.frame(dat))
  structure(dat, class = "vent", bin = unique(dat$bin), baseline = unique(dat$baseline))
}

#' vent validator
#'
#' validate an object of class vent
#'
#' @param dat object
#' @export validate_vent
validate_vent <- function(dat){
   obtained <- as.data.frame(sapply(dat, typeof))

   expected_types <- c("double", "character", "character", "character", "character", "double", "integer", "double",
                     "double",  "double", "integer",  "double", "double")
   expected_names <- c("cpu_date", "subj", "drug", "dose", "unit", "int_min", "measure",
                       "mean", "median", "sd", "n", "baseline", "bin")

   if(!identical(expected_names,row.names(obtained))) {
     stop("Expected columns names are: ", paste(row.names(expected_vent), collapse = ", "))
   }

   if(!identical(expected_vent[[1]], obtained[[1]])){
     stop("Expecting data type: ", paste(expected_vent[[1]], collapse = ", "))
   }

   if (!any(!c("bin", "baseline") %in% names(attributes(dat_vent)))) stop("Attributes bin and/or baseline missing!")

   dat
}



