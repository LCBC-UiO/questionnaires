
#' Turn string time into decimal hour
#'
#' @param x string of class lubridate::hms "HH:MM:SS"
#' @param unit unit to convert to
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' time <- lubridate::hms("02:33:12")
#' time_hms2deciH(time)
#' time_hms2deciH(time, "minute")
time_hms2deciH = function(x, unit="hour"){
  
  unit <- match.arg(unit, c("hour", "minute", "second"))
  
  t = lubridate::hour(x) +
    (lubridate::minute(x)/60)+
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
#' @inheritParams time_hms2deciH
#' @param type hms or hm
#'
#' @return Period
#' @export
#'
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
  
  tmp <- lubridate::seconds_to_period(tmp)
  
  h <- stringr::str_pad(string = round(lubridate::hour(tmp),0), 
                        width = 2, side = "left", pad = "0")
  m <- stringr::str_pad(string = round(lubridate::minute(tmp), 0),
                        width = 2, side = "left", pad = "0")
  s <- stringr::str_pad(string = round(lubridate::second(tmp),0), 
                        width = 2, side = "left", pad = "0")
  
  switch(type,
         "hms" = ifelse(lubridate::minute(tmp) == "NA", NA,
                        paste0(h,":", m,":", s)),
         "hm" = ifelse(lubridate::minute(tmp) == "NA", NA,
                       paste0(h,":", m))
  )
  
}

#' Turn strings of H:M to time
#' 
#' 
#'
#' @inheritParams time_hms2deciH
#' @export
#' @examples 
#' time <- c("02:33")
#' time_alter(time)
#' time_alter(time, "minute")
time_alter = function(x, unit = "minute"){
  t <- lubridate::hm(x)   
  time_hms2deciH(t, unit = unit)
}


#' Factor time of day
#'
#' Takes a vector of HH:MM (HH:MM:SS) information and
#' categorises these by a 4 level factor of time of day.
#'
#' @param x character or hms vector
#'
#' @return factor vector
#' @export
#'
#' @examples
#' time_factor(c("12:23", "15:59", "22:10", "8:13"))
time_factor = function(x) {
  
  DATA2 <- suppressWarnings(dplyr::tibble(Time = lubridate::hms(x)))
  DATA2 <- dplyr::mutate(DATA2,
                  TimeOfDay = dplyr::case_when(
                    is.na(Time@hour) ~ NA_character_,
                    Time@hour >= 5 & Time@hour < 12 ~ "Morning",
                    Time@hour >= 12 & Time@hour < 17 ~ "Afternoon",
                    Time@hour >= 17 & Time@hour < 21 ~ "Evening",
                    TRUE ~ "Night"
                  ),
                  TimeOfDay = factor(TimeOfDay,
                                     levels = c("Morning", "Afternoon", "Evening", "Night"),
                                     ordered = TRUE)
  )
  
  return(DATA2$TimeOfDay)
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
