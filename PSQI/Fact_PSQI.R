Fact_PSQI = function(DATA){
  
  IDX = grep("ID|PSQI",names(DATA))
  
  df = DATA[,IDX]
  
  df$PSQI_1 = as.character(df$PSQI_1)

  df$PSQI_2_scaled = ifelse(df$PSQI_2 >= 15 & df$PSQI_2 <= 30, 1, 
                            ifelse(df$PSQI_2 >= 31 & df$PSQI_2 <= 60,2,
                                   ifelse(df$PSQI_2 >= 60, 3, 0)))
  
  #create the PSQI data frame
  PSQI = data.frame(matrix(ncol=0, nrow=nrow(df)))
  DATA$PSQI_Comp1_SleepQuality     = df$PSQI_6 
  
  PSQI$PSQI_Comp2_sum = df$PSQI_2_scaled + df$PSQI_5a
  DATA$PSQI_Comp2_Latency = ifelse(PSQI$PSQI_Comp2_sum >= 1 & PSQI$PSQI_Comp2_sum <= 2, 1,
                              ifelse(PSQI$PSQI_Comp2_sum >= 3 & PSQI$PSQI_Comp2_sum <= 4, 2, 
                                     ifelse(PSQI$PSQI_Comp2_sum >=5, 3, 0)))
  
  DATA$PSQI_Comp3_Duration = ifelse(df$PSQI_4 > 7, 0,
                               ifelse(df$PSQI_4 >= 6 & df$PSQI_4 <= 7, 1,
                                      ifelse(df$PSQI_4 >= 5 & df$PSQI_4 < 6, 2, 3)))
  
  #There are some issues with importing date-times from SPSS.
  #This part of code tries to work around that to get the correct values.
  tmp = data.frame(matrix(nrow=nrow(PSQI)))
  tmp$Bedtime = Fix_times(df$PSQI_1); tmp$Bedtime = ifelse(tmp$Bedtime<5, tmp$Bedtime+24,tmp$Bedtime)
  tmp$Risingtime = Fix_times(df$PSQI_3); tmp$Risingtime = tmp$Risingtime+24
  tmp$TimeInBed = (tmp$Risingtime-tmp$Bedtime)
  
  PSQI$PSQI_Comp4_Percent = (df$PSQI_4/tmp$TimeInBed)*100
  DATA$PSQI_Comp4_Efficiency = ifelse(PSQI$PSQI_Comp4_Percent > 85, 0,
                                 ifelse(PSQI$PSQI_Comp4_Percent <= 85 & PSQI$PSQI_Comp4_Percent >= 75, 1,
                                        ifelse( PSQI$PSQI_Comp4_Percent < 75 & PSQI$PSQI_Comp4_Percent >= 65, 2, 3)))
  
  # Get all the 5-questions that are not strings.
  tmp = df[, grep("_5", names(df))]
  tmp = tmp[,grep("Coded|Desc",names(tmp))*-1]  
  PSQI$PSQI_Comp_5_sum = ifelse(!is.na(tmp$PSQI_5a),rowSums(tmp[,1:ncol(tmp)], na.rm=T), NA)
    
  DATA$PSQI_Comp_5_Problems = ifelse(PSQI$PSQI_Comp_5_sum == 0, 0,
                                ifelse(PSQI$PSQI_Comp_5_sum >= 1 & PSQI$PSQI_Comp_5_sum < 10, 1,
                                       ifelse(PSQI$PSQI_Comp_5_sum >= 10 & PSQI$PSQI_Comp_5_sum < 19, 2, 3)))
  
  DATA$PSQI_Comp6_Medication = df$PSQI_7
  
  PSQI$PSQI_Comp7_sum = rowMeans(cbind(df$PSQI_8,df$PSQI_9))*2
  DATA$PSQI_Comp7_Tired = ifelse(PSQI$PSQI_Comp7_sum == 0, 0,
                            ifelse(PSQI$PSQI_Comp7_sum > 0 & PSQI$PSQI_Comp7_sum <= 2, 1,
                                   ifelse(PSQI$PSQI_Comp7_sum > 2 & PSQI$PSQI_Comp7_sum <= 4, 2, 3)))
  
  #Remove temporary columns
  DATA$PSQI_Global = rowSums(DATA[,grep("PSQI_Comp",names(DATA))])
  

  #Return the Data with the new values appended.
  return(DATA)
}
