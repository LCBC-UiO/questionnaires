hms2deciH = function(x, type="hour"){
  x = hour(x) +
    (minute(x)/60)+
    (second(x)/120)
  
  switch(type,
         "hour" = x,
         "minute" = x*60,
         "second" = x*60*60)
}


alter_time = function(x, type="minute"){
  if(grepl(":",x)){
    x = if(x %>% str_count(":") == 1){
      x %>% hm() %>% hms2deciH(type)
    }else{
      x %>% hms() %>% hms2deciH(type)
    }
  }
  as.character(x)
}

rowProds <- function(data){
  apply(data, 1, prod)
}

rowSum <- function(data){
  apply(data, 1, sum)
}


ipaq_alter_time <- function(data, cols = c(IPAQ_2, IPAQ_4, IPAQ_6)){
  cols <- enquo(cols)
  
  mutate_at(data,
            vars(!!cols),
            funs(as.numeric(alter_time(., type="minute")))
  ) 
  
}

ipaq_compute_met <- function(minutes = IPAQ_2,
                             days = IPAQ_1b,
                             met = 3.3){
  minutes*days*met
  
}


ipaq_compute_sum <- function(data, cols = c(IPAQ_MET_Vigorous,IPAQ_MET_Moderate, IPAQ_MET_Light)){
  cols <- enquo(cols)
  
  tmp <- select(data, !!cols)
  rowSums(tmp)
}


#' @param keep_all logical, append to data.frame

ipaq_compute <- function(ipaq_sum, 
                         alter_time_cols = NULL,
                         mets = list(light = 3.3, moderate = 4.0, vigorous = 8.0),
                         light_days = IPAQ_1b,
                         light_mins = IPAQ_2,
                         mod_days = IPAQ_3b,
                         mod_mins = IPAQ_4,
                         vig_days = IPAQ_5b,
                         vig_mins = IPAQ_6,
                         keep_all = TRUE){
  
  light_mins <- enquo(light_mins)
  light_days <- enquo(light_days)
  
  mod_mins <- enquo(mod_mins)
  mod_days <- enquo(mod_days)
  
  vig_mins <- enquo(vig_mins)
  vig_days <- enquo(vig_days)
  
  if(!is.null(alter_time_cols)){
    alter_time_cols <- enqou(alter_time_cols)
    tmp <- ipaq_alter_time(data, !!alter_time_cols)
  }else{
    tmp <- data
  }
  
  tmp <- transmute(tmp,
                   IPAQ_MET_Vigorous = ipaq_compute_met(minutes = !!vig_mins, days = !!vig_days, mets$vigorous),
                   IPAQ_MET_Moderate = ipaq_compute_met(minutes = !!mod_mins, days = !!mod_days, mets$moderate),
                   IPAQ_MET_Light = ipaq_compute_met(minutes = !!light_mins, days = !!light_days, mets$light)
  )
  tmp <- mutate(tmp, IPAQ_MET = rowSums(tmp))
  
  # Create a some validator vectors, used for the MET factorial
  valid1 <- select(data, !!light_mins, !!mod_mins, !!vig_mins)
  valid1 <- rowSums(valid1)
  
  valid2 <- select(data, !!light_mins, !!mod_mins)
  valid2 <- rowSums(valid2)
  
  q5 <- unlist(select(data, !!vig_mins))
  q3 <- unlist(select(data, !!mod_mins))
  q1 <- unlist(select(data, !!light_mins))
  q6 <- unlist(select(data, !!vig_days))
  
  tmp <- mutate(tmp, 
                IPAQ_Coded = ifelse( (IPAQ_MET_Vigorous >= 1500 & !!vig_days >= 3) | 
                                       (valid1 >= 7 & IPAQ_MET >= 3000) ,
                                     "High",
                                     ifelse((!!vig_mins >= 20 & !!vig_days >= 3) | 
                                              (valid2 >= 5 & (!!light_days >= 30 | !!mod_days >= 30) ), 
                                            "Moderate",
                                            "Low")))
  
  if(keep_all){
    bind_cols(select(data, -one_of(names(data)[names(data) %in% names(tmp)])), 
              tmp)
  }else{
    tmp
  }
  
}


