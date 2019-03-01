
time_hms2deciH = function(x, type="hour"){
  x = lubridate::hour(x) +
    (lubridate::minute(x)/60)+
    (lubridate::second(x)/120)
  
  switch(type,
         "hour" = x,
         "minute" = x*60,
         "second" = x*60*60)
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

time_alter = function(x, type = "minute"){
  if(grepl(":",x)){
    x = if(x %>% stringr::str_count(":") == 1){
      x %>% lubridate::hm() %>% time_hms2deciH(type)
    }else{
      x %>% lubridate::hms() %>% time_hms2deciH(type)
    }
  }
  as.character(x)
}

is.hms = function(x) any(class(x) %in% "hms")

