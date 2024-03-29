% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ipaq-funcs.R
\name{ipaq_time_alter}
\alias{ipaq_time_alter}
\title{Alter the time instant columns to decminals}
\usage{
ipaq_time_alter(data, cols = c(ipaq_2, ipaq_4, ipaq_6, ipaq_7))
}
\arguments{
\item{data}{data with columns to alter}

\item{cols}{columns to alter, in tidyselect format}
}
\value{
data.frame
}
\description{
Time is often punched as HH:MM in order
to preserve correct time calculations.
The ipaq calculation recure time to be
in decimal minutes.
This function easily changes HH:MM into
decminal minutes in a data.frame
It alters columns directly in the data.frame
}
\examples{
dat <- data.frame(
   time_1  = c("12:34", "09:33", "22:14"),
   time_2  = c("10:55", "16:45", "18:02")
)
ipaq_time_alter(dat, cols = c(time_1, time_2))
}
\seealso{
Other ipaq_functions: 
\code{\link{ipaq_compute_met}()},
\code{\link{ipaq_mets}()}
}
\concept{ipaq_functions}
