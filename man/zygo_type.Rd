% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zygocity-funcs.R
\name{zygo_type}
\alias{zygo_type}
\title{Find how many twins have answered}
\usage{
zygo_type(data, twin_col, cols = starts_with("zygo"))
}
\arguments{
\item{data}{dara.frame with the relevant data}

\item{twin_col}{column that codes for twin pairs. Each twin
should have the same identifier here.}

\item{cols}{columns that contain the zygocity data. Use tidy-selectors}
}
\value{
full data frame with twin type appended
}
\description{
The zygocity calculations are different depending
on wheather both twins have answered the
questionnaire or not. This convenience function
help determine, based on the column coding for
twin pairs, if one or two twins are present in
the data with complete viable data.
If both twins are in the data, but one twin has
incomplete data, the function will return
"single" for the remaining twin.
}
