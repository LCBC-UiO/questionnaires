fact_TAS = function(data){
  
  reverse = function(x) (x-6)*-1
  
  data %>% 
    #select(starts_with("TAS")) %>% 
    
    # Some questions are negatively keyed, We switch these over
    mutate_at(vars(TAS_04, TAS_05, TAS_10,TAS_18), reverse) %>% 
    
    # Sum the into the factors
    mutate(
      TAS_Fact1_Ident.Feelings = rowSums(select(., c(TAS_01,TAS_03,TAS_06,TAS_07,TAS_09,TAS_13,TAS_14))), 
      TAS_Fact2_Descr.Feelings = rowSums(select(., c(TAS_02,TAS_04,TAS_11,TAS_12,TAS_17))), 
      TAS_Fact3_Ext.O.Thinking = rowSums(select(., c(TAS_05,TAS_08,TAS_10,TAS_15,TAS_16,TAS_18,TAS_19,TAS_20)) ) 
    )
}
