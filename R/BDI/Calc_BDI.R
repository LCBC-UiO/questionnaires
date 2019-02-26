calc_BDI = function(data){
  
  data %>%

    # If raw BDI is punched, calculate the sum
    mutate(BDI = ifelse(!is.na(BDI_01), 
                        rowSums(select(.,matches("BDI_[0-9][0-9]$")), na.rm=T), 
                        BDI)) %>%
    
    # If there is a sum, categorise it according to original paper.
    mutate(BDI_Coded = ifelse(BDI<=13, "Minimal depression",
                         ifelse(BDI>=14 & BDI <=19, "Mild depression",
                                ifelse(BDI>=20 & BDI <= 28, "Moderate depression",
                                       "Severe depression")))
    )
}
