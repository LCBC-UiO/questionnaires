% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/income-funcs.R
\name{income_bin2nok}
\alias{income_bin2nok}
\title{Turn income bins to mean of bin}
\usage{
income_bin2nok(x)
}
\arguments{
\item{x}{income bin vector}
}
\value{
numeric
}
\description{
Older collected income data for LCBC
collected income information in 7 bins.
Newer data collects continuous income data.
This function converts binned income data
from these 7 categories into the mean
income value for each bin.
}
\examples{
x <- c("< 200k", "600k - 699k", "400k - 499k", 
       "> 700k", "500k - 599k", "300k - 399k", 
        "200k - 299k")
income_bin2nok(x)
}
