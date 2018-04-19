Calc_BDI = function(DATA){
  
  BDI = DATA %>%
    select(matches("BDI_[0-9][0-9]$")) %>%
    mutate(BDI = ifelse(!is.na(BDI_01), rowSums(., na.rm=T), DATA$BDI)) %>%
    mutate(BDI_Coded = ifelse(BDI<=13, "Minimal depression",
                         ifelse(BDI>=14 & BDI <=19, "Mild depression",
                                ifelse(BDI>=20 &BDI <= 28, "Moderate depression",
                                       "Severe depression")))
    )
  
  DT = DATA %>%
    select(-contains("BDI")) %>%
    bind_cols(BDI)
  
  return(DT)
}
