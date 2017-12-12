Fact_IPAQ = function(DATA){
  require(tidyverse)
  
  METS = c(Light = 3.3, Moderate = 4.0, Vigorous = 8.0)
  
  # Converts decimal hours to minutes
  DATA1 = DATA %>% 
    select(CrossProject_ID,Subject_Timepoint, contains("IPAQ")) %>% 
    mutate(IPAQ_MET.Vigorous = IPAQ_2*60*IPAQ_1b*METS["Vigorous"],
           IPAQ_MET.Moderate = IPAQ_4*60*IPAQ_3b*METS["Moderate"],
           IPAQ_MET.Light    = IPAQ_6*60*IPAQ_5b*METS["Light"]) %>% 
    select(-matches("IPAQ_[246]M$"))
  DATA1$IPAQ_MET = ifelse(apply(DATA1 %>% select(IPAQ_MET.Light, IPAQ_MET.Moderate, IPAQ_MET.Vigorous),1, function(x) !all(is.na(x))),
                          rowSums(DATA1 %>% select(IPAQ_MET.Light,IPAQ_MET.Moderate,IPAQ_MET.Vigorous), na.rm=T), NA )
    
  DATA1$DELETE1 = ifelse(apply(DATA1 %>% select(IPAQ_1b,IPAQ_3b,IPAQ_5b),1, function(x) !all(is.na(x))),
                          rowSums(DATA1 %>% select(IPAQ_1b,IPAQ_3b,IPAQ_5b), na.rm=T), NA )
  
  DATA1$DELETE2 = ifelse(apply(DATA1 %>% select(IPAQ_1b,IPAQ_3b),1, function(x) !all(is.na(x))),
                         rowSums(DATA1 %>% select(IPAQ_1b,IPAQ_3b), na.rm=T), NA )

  
  # Categorise the data
  DATA1$IPAQ_Category = ifelse( (DATA1$IPAQ_MET.Vigorous >= 1500 & DATA1$IPAQ_5b>=3) |
                                  (DATA1$DELETE1>=7 & DATA1$IPAQ_MET >= 3000) ,
                                   "High",ifelse((DATA1$IPAQ_6>=20 & DATA1$IPAQ_5b>=3) | 
                                                   (DATA1$DELETE2>=5 & (DATA1$IPAQ_1b>=30 | DATA1$IPAQ_3b>=30 ) ), 
                                                 "Moderate","Low")) 
  
  DATA2 = DATA %>% left_join(DATA1, by=c("CrossProject_ID","Subject_Timepoint")) %>% 
    select(-matches("\\.x$"), contains("DELETE[12]"))
  names(DATA2) = gsub("\\.y$", "", names(DATA2))
  
  return(DATA2)
}
