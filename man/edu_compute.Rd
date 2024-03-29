% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edu-funcs.R
\name{edu_compute}
\alias{edu_compute}
\title{Fill inn Education in MOAS}
\usage{
edu_compute(
  data,
  edu4 = edu_coded4,
  edu9 = edu_coded10,
  edu_years = edu_years,
  prefix = "edu_",
  keep_all = TRUE
)
}
\arguments{
\item{data}{MOAS-like data}

\item{edu4}{unquoted column containing Education coded in 4 categories}

\item{edu9}{unquoted column containing Education coded in 4 categories}

\item{edu_years}{unquoted column containing Education in years to highest completed}

\item{prefix}{string to prefix column names of computed values}

\item{keep_all}{logical, append to data.frame}
}
\value{
a data.frame
}
\description{
Using existing data in the MOAS, fills in gaps,
converts from on type of coding to another etc.
}
\examples{

edu <- data.frame(
    edu4 = c("3", "High school", 1, NA,
         "University/University college (> 4 years)", NA, 
          "University/University college (< 4 years)"),
    edu9 = c(7,7,8,NA,"Primary school (6 years)",5, 9),
    edu_years = c(NA, 12, 9, NA, 19, 19, NA),
    mother = c("3", "High school", 1, NA,
               "University/University college (> 4 years)",
               "University/University college (> 4 years)", 
               "University/University college (< 4 years)"),
    father = c(7,7,8,4,"Primary school (6 years)",5, 10),
    stringsAsFactors = FALSE
    )
 
 edu_compute(edu,
             edu4 = edu4,
             edu9 = edu9, 
             edu_years = edu_years)
}
\seealso{
Other edu_functions: 
\code{\link{edu_compile}()},
\code{\link{edu_factorise}()},
\code{\link{edu_levels2name}()},
\code{\link{edu_levels}()},
\code{\link{edu_reduce}()},
\code{\link{edu_to_years}()}
}
\concept{edu_functions}
