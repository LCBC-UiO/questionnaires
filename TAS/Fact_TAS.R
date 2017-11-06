Fact_TAS = function(DATA){
  
  df = DATA[, grep("TAS", names(DATA))]
  
  # Some questions are negatively keyed, We switch these over
  for(i in c(4,5,10,18))  df[,i] = (df[,i]-6)*-1

  DATA$TAS_Fact1_Ident.Feelings = rowSums(df[,c(1,3,6,7,9,13,14)], na.rm = T)
  DATA$TAS_Fact2_Descr.Feelings = rowSums(df[,c(2,4,11,12,17)], na.rm = T)
  DATA$TAS_Fact3_Ext.O.Thinking = rowSums(df[,c(5,8,10,15,16,18,19,20)], na.rm = T)

  return(DATA)
}