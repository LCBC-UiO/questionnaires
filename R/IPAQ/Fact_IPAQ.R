fact_IPAQ = function(data){
  #### --- Special function to go from hms to decimal hours --- ####
  hms2deciH = function(x, type="hour"){
    x = hour(x) +
      (minute(x)/60)+
      (second(x)/120)
    
    switch(type,
           "hour" = x,
           "minute" = x*60,
           "second" = x*60*60)
  }
  
  # Find instances where time is punched as a time, not decimal, and convert them
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
  
  # Check if all cols are NA, else sum them.
  sum.cols = function(data, cols){
    ifelse(apply(data %>% select(!!cols),1, function(x) !all(is.na(x))),
           rowSums(data %>% select(!!cols)), NA )
  }
  
  
  METS = c(Light = 3.3, Moderate = 4.0, Vigorous = 8.0)
  
  # DATA1 = data %>% 
  #   select(CrossProject_ID,Subject_Timepoint, contains("IPAQ")) %>% 
  #   filter(!is.na(IPAQ_1a)) %>% 
  #   distinct()
  DATA1 = data
  
  # Find instances where time is punched as a time, not decimal, and convert them to minutes
  DATA1 = suppressWarnings(
    DATA1 %>% 
      mutate_at(vars(IPAQ_2, IPAQ_4, IPAQ_6), .funs = funs( M= alter_time(., type="minute") %>% as.numeric())) %>% 
      mutate(IPAQ_MET.Vigorous = IPAQ_2_M*IPAQ_1b*METS["Vigorous"],
             IPAQ_MET.Moderate = IPAQ_4_M*IPAQ_3b*METS["Moderate"],
             IPAQ_MET.Light    = IPAQ_6_M*IPAQ_5b*METS["Light"]) %>% 
      mutate(IPAQ_MET = sum.cols(., cols = c("IPAQ_MET.Vigorous", "IPAQ_MET.Moderate", "IPAQ_MET.Light" )),
             DELETE1 = sum.cols(., cols = c("IPAQ_1b","IPAQ_3b","IPAQ_5b")),
             DELETE2 = sum.cols(., cols = c("IPAQ_1b","IPAQ_3b"))) %>% 
      
      # Categorise based on calculations and answers
      mutate(
        IPAQ_Category = ifelse( (IPAQ_MET.Vigorous >= 1500 & IPAQ_5b>=3) | (DELETE1>=7 & IPAQ_MET >= 3000) ,
                                "High",
                                ifelse((IPAQ_6>=20 & IPAQ_5b>=3) | (DELETE2>=5 & (IPAQ_1b>=30 | IPAQ_3b>=30) ), 
                                       "Moderate",
                                       "Low"))) %>% 
      select(-contains("DELETE"), -ends_with("_M"))
  )
  
  
  return(DATA1)
}
