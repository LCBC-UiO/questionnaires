fact_EHI = function(data){
  
  data %>% 
    
    # Caclulate the sum of positive and negative values
    mutate(POS = apply(select(.,matches("^EHI_[0-9][0-9]$")), 1:2, 
                       function(x) ifelse(x > 0, x, 1)) %>% abs %>% rowSums,
           NEG = apply(select(.,matches("^EHI_[0-9][0-9]$")), 1:2, 
                       function(x) ifelse(x < 0, x, 1)) %>% abs %>% rowSums) %>%
    
    # Calculate the laterality quotient by taking the positive sum minus the negative, dividing by their sum and multiplying by 100
    mutate(EHI_LE = ((POS-NEG)/(POS+NEG))*100) %>%
    
    # Grab the answer from the first question (writing), and turn into nominal scale
    mutate(EHI_Nominal = ifelse(EHI_01 > 0, "Right", 
                                ifelse(EHI_01 == 0, "Ambidexterous", 
                                       "Left"))) %>%
    
    # Code into Left or Right according to quotient, ambi is not allowed. See Oldfield paper.
    mutate(EHI_Coded = ifelse(EHI_LE >= 0,"Right","Left")) %>%
     
    # Remove columns no longer necessary
    select(-POS, -NEG)

}
