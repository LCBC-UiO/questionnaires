Fact_IPAQ = function(DATA){
  require(tidyverse)
  
  METS = c(Light = 3.3, Moderate = 4.0, Vigorous = 8.0)
  
  # Converts decimal hours to minutes
  DATA1 = DATA %>% 
    select(CrossProject_ID,Subject_Timepoint,Weight, contains("IPAQ")) %>% 
    mutate(IPAQ_2 = IPAQ_2*60,
                   IPAQ_4 = IPAQ_4*60,
                   IPAQ_6 = IPAQ_6*60) %>% 
    mutate(IPAQ_MET.Light = IPAQ_2*IPAQ_1*METS["Light"],
           IPAQ_MET.Moderate = IPAQ_2*IPAQ_1*METS["Moderate"],
           IPAQ_MET.Vigorous = IPAQ_2*IPAQ_1*METS["Vigorous"]) %>% 
    mutate(IPAQ_MET = IPAQ_MET.Light+IPAQ_MET.Moderate+IPAQ_MET.Vigorous)
  
  # Categorise the data
  DATA1$IPAQ_Category = ifelse( (DATA1$IPAQ_MET.Vigorous >= 1500 & DATA1$IPAQ_5>=3) |
                                  (DATA1$IPAQ_1+DATA1$IPAQ_3+DATA1$IPAQ_5>=7 & DATA1$IPAQ_MET >= 3000) ,
                                   "HEPA active",ifelse((DATA1$IPAQ_6>=20 & DATA1$IPAQ_5>=3) | 
                                                   (DATA1$IPAQ_1+DATA1$IPAQ_3>=5 & (DATA1$IPAQ_1>=30 | DATA1$IPAQ_3>=30 ) ), 
                                                 "Minimally active","Inactive")) 
  
  DATA2 = DATA %>% left_join(DATA1, by=c("CrossProject_ID","Subject_Timepoint")) %>% 
    select(-matches("\\.x$"))
  names(DATA2) = gsub("\\.y$", "", names(DATA2))
  
  return(DATA2)
}
