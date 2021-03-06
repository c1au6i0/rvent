% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_session.R
\name{import_session}
\alias{import_session}
\title{Import iox session.}
\usage{
import_session(
  iox_data,
  baseline = 30,
  inter = TRUE,
  comments_tsd,
  tofill = NULL
)
}
\arguments{
\item{iox_data}{path to folder containing data or, list of files to import when `shin_f = TRUE`.}

\item{baseline}{length of baseline in minutes.}

\item{inter}{logical for using or not dialogs to input data and select folders.}

\item{comments_tsd}{vector of comments that contain doses (used if inter = FALSE).}

\item{tofill}{vector of values to replace missing data in comments_tsd (used if inter = FALSE).}
}
\description{
Imports txt files created by the software SCIREQ and return a list of dataframes.
}
\seealso{
normalizetime_vent(), import_session(), split_comments().
}
