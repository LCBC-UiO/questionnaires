
time_hms2deciH = function(x, unit="hour"){
  t = lubridate::hour(x) +
    (lubridate::minute(x)/60)+
    (lubridate::second(x)/120)
  
  switch(unit,
         "hour" = t,
         "minute" = t*60,
         "second" = t*60*60)
}


time_deci2period <- function(x, unit = "hour", type = "hm"){
  
  tmp <- switch(unit,
                "hour" = x*60*60,
                "minute" = x*60,
                "second" = x)
  
  tmp <- lubridate::seconds_to_period(tmp)
  
  h <- stringr::str_pad(string = round(lubridate::hour(tmp),0), width = 2, side = "left", pad = "0")
  m <- stringr::str_pad(string = round(lubridate::minute(tmp), 0), width = 2, side = "left", pad = "0")
  s <- stringr::str_pad(string = round(lubridate::second(tmp),0), width = 2, side = "left", pad = "0")
  
  switch(type,
         "hms" =     ifelse(lubridate::minute(tmp) == "NA", NA,
                            paste0(h,":", m,":", s)),
         "hm" =     ifelse(lubridate::minute(tmp) == "NA", NA,
                           paste0(h,":", m))
  )
  
}

time_alter = function(x, unit = "minute"){
  t <- lubridate::hm(x)   
  time_hms2deciH(t, unit = unit)
}

is.hms = function(x) any(class(x) %in% "Period")
is.hm = function(x) any(class(x) %in% "Period")

