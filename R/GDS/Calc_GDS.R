Calc_GDS = function(DATA){

  tmp = DATA %>% select(matches("GDS_[0-9][0-9]$"))
  
  Neg = tmp %>%
    select(matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$")) %>%
    apply(1,function(x) ifelse(x=="Yes",0,1)) %>% t() %>%
    as.data.frame()
  
  Pos = tmp %>%
    select(-matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$")) %>%
    apply(1,function(x) ifelse(x=="No",0,1)) %>% t() %>%
    as.data.frame()
  
  GDS = bind_cols(Neg,Pos) %>%
    mutate(GDS = ifelse(is.na(GDS_01), NA, rowSums(.,na.rm=T))) %>%
    mutate(GDS_Coded = ifelse((GDS <= 9), "Normal", 
                              ifelse((10 <= GDS & GDS <= 19), "Mild depression",
                                     "Severe deppression")))
  
  DT = DATA %>%
    select(-contains("GDS")) %>%
    bind_cols(GDS)
  
  return(DT)
}
