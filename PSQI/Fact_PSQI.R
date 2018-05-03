Fact_PSQI = function(DATA, Q4="dec"){
  requireNamespace("tidyverse"); requireNamespace("lubridate"); requireNamespace("stringr")
  
  DATA2 = DATA
  
  #### --- Special function to go from hms to decimal hours --- ####
  hms2deciH = function(x){
    require(lubridate)
    t = hour(x)+
      (minute(x)/60)+
      (second(x)/120)
    return(t)
  }
  
  #### --- Prepping DATA2 --- ####
  # Make sure SPSS has not added strange ".0" the the end of timestamp.
  # Assumes Q1 and Q3 are HH:MM:SS
  DATA2$PSQI_01 = DATA2$PSQI_01 %>% 
    as.character() %>% 
    gsub(pattern = "\\.0$",replacement = "", .)
  
  DATA2$PSQI_03 = DATA2$PSQI_03 %>% 
    as.character() %>% 
    gsub(pattern = "\\.0$",replacement = "", .) 

  
  df = DATA2 %>% select(contains("PSQI"))
  
  for(k in c("PSQI_01", "PSQI_03")){
    df[,k] = if_else(str_count(df[,k] ,":")>1, 
                        hms(df[,k], quiet = T ), hm(df[,k], quiet = T))
  }
  
  df$PSQI_02 = as.numeric(df$PSQI_02)
  # If Q4 is punched as HH:MM, convert to decminal hours
  # if(Q4=="time")
  #   df$PSQI_04 = df$PSQI_04 %>% hms() %>% hms2deciH()
  
  # Find instances where time is punched as a time, not decimal, and convert them
  for(i in grep(":",df$PSQI_04)){
    df$PSQI_04[i] = if(df$PSQI_04[i] %>% str_count(":") == 1){
      df$PSQI_04[i] %>% hm() %>% hms2deciH()
    }else{
      df$PSQI_04[i] %>% hms() %>% hms2deciH()
    }
  }
  
  df$PSQI_04 = df$PSQI_04 %>% as.numeric()
  
  
  # Scale Q2
  df$PSQI_02_scaled = ifelse(df$PSQI_02 >= 15 & df$PSQI_02 <= 30, 1, 
                             ifelse(df$PSQI_02 >= 31 & df$PSQI_02 <= 60,2,
                                    ifelse(df$PSQI_02 >= 60, 3, 0)))
  
  
  # Works around varying punching of after midnight bedtimes (added 24hr for easy calculations)
  df$Bedtime = if_else(hour(df$PSQI_01)<=5, df$PSQI_01+hours(24), df$PSQI_01) %>% period_to_seconds
  
  # Adds 24hr to rising time, for easy calculation of time spent in bed
  df$Risingtime = df$PSQI_03+hours(24) %>% period_to_seconds()
  
  # Calculate time spend in bed from bedtime and rising time
  df$TimeInBed = (df$Risingtime-df$Bedtime) %>% 
    as.period(unit = "hours") %>% hms2deciH()
  
  # If Hours of slwwp (Q4) is not answered, calculate it by timeinbed-minutes to sleep.
  df$PSQI_04 = ifelse(!is.na(df$PSQI_04), df$PSQI_04, 
                      ifelse(!is.na(df$PSQI_02),df$TimeInBed-(df$PSQI_02/60),
                             df$TimeInBed))
  
  #### --- Calculate components --- ####
  
  # Component 1 is assessment of sleep quality in Q6
  DATA2$PSQI_Comp1_SleepQuality     = df$PSQI_06 
  
  # Component 2 is a latency score, the sum of scaled Q2 with Q5a, and then scaled
  df$PSQI_Comp2_sum = df$PSQI_02_scaled + df$PSQI_05a
  DATA2$PSQI_Comp2_Latency = ifelse(df$PSQI_Comp2_sum >= 1 & df$PSQI_Comp2_sum <= 2, 1,
                                   ifelse(df$PSQI_Comp2_sum >= 3 & df$PSQI_Comp2_sum <= 4, 2, 
                                          ifelse(df$PSQI_Comp2_sum >=5, 3, 0)))
  
  # Component 3 is a scaling of sleep duration, if q4 is NA, calculate sleep by timeinbed-minutes to sleep
  DATA2$PSQI_Comp3_Duration = ifelse(df$PSQI_04 > 7, 0,
                                    ifelse(df$PSQI_04 >= 6 & df$PSQI_04 <= 7, 1,
                                           ifelse(df$PSQI_04 >= 5 & df$PSQI_04 < 6, 2, 3)))
  
  # Component 4 is a scaled indicator of sleep efficiency
  df$PSQI_Comp4_Percent = (df$PSQI_04/df$TimeInBed)*100
  DATA2$PSQI_Comp4_Efficiency = ifelse(df$PSQI_Comp4_Percent > 85, 0,
                                      ifelse(df$PSQI_Comp4_Percent <= 85 & df$PSQI_Comp4_Percent >= 75, 1,
                                             ifelse( df$PSQI_Comp4_Percent < 75 & df$PSQI_Comp4_Percent >= 65, 2, 3)))
  
  # Get all the 5-questions that are not strings. This assumes all the Q5's column names end
  # with "_5" and the characters a-j in small case
  tmp = df %>% select(matches("_05[a-j]$"))
  df$PSQI_Comp_5_sum = ifelse(!is.na(tmp$PSQI_05a),rowSums(tmp[,1:ncol(tmp)], na.rm=T), NA)
  
  DATA2$PSQI_Comp5_Problems = ifelse(df$PSQI_Comp_5_sum == 0, 0,
                                    ifelse(df$PSQI_Comp_5_sum >= 1 & df$PSQI_Comp_5_sum < 10, 1,
                                           ifelse(df$PSQI_Comp_5_sum >= 10 & df$PSQI_Comp_5_sum < 19, 2, 3)))
  
  DATA2$PSQI_Comp6_Medication = df$PSQI_07
  
  df$PSQI_Comp7_sum = rowMeans(cbind(df$PSQI_08,df$PSQI_09))*2
  DATA2$PSQI_Comp7_Tired = ifelse(df$PSQI_Comp7_sum == 0, 0,
                                 ifelse(df$PSQI_Comp7_sum > 0 & df$PSQI_Comp7_sum <= 2, 1,
                                        ifelse(df$PSQI_Comp7_sum > 2 & df$PSQI_Comp7_sum <= 4, 2, 3)))
  
  DATA2$PSQI_Global = DATA2 %>% select(contains("PSQI_Comp")) %>% rowSums()
  
  #Return the entire incoming DATA2 with the new values added
  return(DATA2)
}
