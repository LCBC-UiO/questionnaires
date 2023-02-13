
#' Turn string time into decimal
#'
#' @param x string of class lubridate::hms "HH:MM:SS"
#' @param unit unit to convert to
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' time <- lubridate::hms("02:33:12")
#' time_hms2deci(time)
#' time_hms2deci(time, "minute")
time_hms2deci = function(x, unit="hour"){
  unit <- match.arg(unit, c("hour", "minute", "second"))
  t  <- lubridate::hour(x) +
    (lubridate::minute(x)/60) +
    (lubridate::second(x)/120)
  switch(unit,
         "hour" = t,
         "minute" = t*60,
         "second" = t*60*60)
}


#' Turn time into period
#' 
#' Turn strings in class hms into
#' periods of time.
#'
#' @inheritParams time_hms2deci
#' @param type hms or hm
#'
#' @return Period
#' @export
#' @importFrom lubridate seconds_to_period second minute hour
#' @examples
#' time_deci2period(8.5)
#' time_deci2period(1.25, "minute")
time_deci2period <- function(x, unit = "hour", type = "hm"){
  unit <- match.arg(unit, c("hour", "minute", "second"))
  type <- match.arg(type, c("hms", "hm"))
  
  tmp <- switch(unit,
                "hour" = x * 60 * 60,
                "minute" = x * 60,
                "second" = x)
  
  tmp <- seconds_to_period(tmp)
  h <- sprintf("%02d", round(hour(tmp),0))
  m <- sprintf("%02d", round(minute(tmp),0))
  s <- sprintf("%02d", round(second(tmp),0))
  
  switch(type,
         "hms" = ifelse(lubridate::minute(tmp) == "NA", NA,
                        paste0(h,":", m,":", s)),
         "hm" = ifelse(lubridate::minute(tmp) == "NA", NA,
                       paste0(h,":", m))
  )
  
}

#' Turn strings of H:M to time
#'
#' @inheritParams time_hms2deci
#' @template time_func
#' @export
#' @importFrom lubridate hm
#' @examples 
#' time <- c("02:33")
#' time_alter(time)
#' time_alter(time, "minute")
time_alter = function(x, unit = "minute", time_func = lubridate::hm){
  t <- suppressWarnings(time_func(x))
  time_hms2deci(t, unit = unit)
}


#' Factor time of day
#'
#' Takes a vector of HH:MM (HH:MM:SS) information and
#' categorizes these by a 4 level factor of time of day.
#'
#' @param x character vector of times
#' @template time_func
#' @param tod list defining when the breakpoints for the various
#'     time of day distinctions.
#'
#' @return factor vector
#' @export
#' @importFrom dplyr mutate case_when tibble
#' @importFrom lubridate hms
#' @examples
#' time_factor(c("12:23", "15:59", "22:10", "8:13"))
time_factor = function(x, time_func = lubridate::hms, tod = time_of_day()) {
  dt <- tibble(x = x)
  dt <- mutate(dt,
               time = time_func(x),
               tod = case_when(
                 is.na(time) ~ NA_character_,
                 hour(time) >= tod$morning[1]   & hour(time) < tod$morning[2] ~ "morning",
                 hour(time) >= tod$afternoon[1] & hour(time) < tod$afternoon[2] ~ "afternoon",
                 hour(time) >= tod$evening[1]   & hour(time) < tod$evening[2] ~ "evening",
                 TRUE ~ "night"
               ),
               todf = factor(tod,
                            levels = c("morning", "afternoon", "evening", "night"),
                            ordered = TRUE)
  )
  return(dt$todf)
}

#' Create list of time of day break points
#'
#' @param morning vector of two for the hours where morning start or end in 24H 
#' @param afternoon vector of two for the hours where afternoon start or end in 24H
#' @param evening vector of two for the hours where evening start or end in 24H
#'
#' @return list of fours times of day classifying the 24H of the day
#' @export
#'
#' @examples
#' time_of_day()
time_of_day <- function(morning = c(5, 12), 
                        afternoon = c(12, 17), 
                        evening = c(17, 21)){
  list(morning = morning,
       afternoon = afternoon,
       evening = evening,
       night = c(max(evening), min(morning)))
  
}


#' Utility function to locate hms columns
#'
#' \code{is_hms} locates columns that are time (hms) classes
#'
#' @param x vector
#'
#' @return logical vector of length==ncol(data)
#' @examples
#' \dontrun{
#' is_hms(data)
#' }
#' @export
is_hms = function(x){
  k <- any(class(x) %in% "Period")
  k2 <- any(class(x) %in% "hms")
  any(k, k2)
}


#' Utility function to locate hm columns
#'
#' \code{is_hm} locates columns that are time (hm) classes
#'
#' @param x vector
#'
#' @return logical vector of length==ncol(data)
#' @examples
#' \dontrun{
#' is_hm(data)
#' }
#' @export
is_hm = function(x){
  k <- any(class(x) %in% "Period")
  k2 <- any(class(x) %in% "hm")
  any(k, k2)
}

## quiets concerns of R CMD check
if(getRversion() >= "2.15.1"){
  utils::globalVariables(c("TimeOfDay"))
}
