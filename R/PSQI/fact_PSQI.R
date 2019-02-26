fact_PSQI = function(data, keep.intermediate=F){
  
  #### --- Special function to go from hms to decimal hours --- ####
  hms2deciH = function(x, type="hour"){
    x = hour(x) +
      (minute(x)/60)+
      (second(x)/120)
    
    switch(type,
           "hour" = x,
           "minute" = x*60,
           "second" = x*60*60)
  }
  
  # Find instances where time is punched as a time, not decimal, and convert them
  alter_time = function(x, type="hour"){
    if(grepl(":",x)){
      x = if(x %>% str_count(":") == 1){
        x %>% hm() %>% hms2deciH(type)
      }else{
        x %>% hms() %>% hms2deciH(type)
      }
    }
    as.character(x)
  }
  
  # df = data %>% 
  #   filter(!is.na(PSQI_01) ) %>% 
  #   #filter(PSQI_01 =! " ") %>% 
  #   select(contains("PSQI"))
  df = data
  
  # Questions 2 and 4 are in the nettskjema answered as HH:MM, while they should be in minutes and hours respectively. 
  # Alter whichever rows this may pertain to, before forcing them to be numeric.
  df$PSQI_02 = as.numeric(sapply(df$PSQI_02, alter_time, type="minute"))
  df$PSQI_04 = sapply(df$PSQI_04, alter_time) %>% as.numeric()
  
  
  #### --- Prepping df --- ####
  df = suppressWarnings(
    df %>% 
      
      # Force time into character to control it
      mutate(PSQI_01 = as.character(PSQI_01),
             PSQI_03 = as.character(PSQI_03)) %>% 
      
      # Some may have entered minutes into the "hour" slot, convert it to minutes
      mutate(PSQI_02 = ifelse(PSQI_02 > 180, PSQI_02/60, PSQI_02)) %>% 
      
      # Count occurence of ":" to find if it is hm or hms
      mutate(c01 = str_count(PSQI_01,":"),
             c03 = str_count(PSQI_03,":")) %>% 
      
      # Add an extra ":00" to those with only hm
      mutate(PSQI_01 = ifelse(c01 == 1, paste0(PSQI_01, ":00"), PSQI_01),
             PSQI_03 = ifelse(c03 == 1, paste0(PSQI_03, ":00"), PSQI_03)) %>% 
      
      # Get them into hms format (with workaround because of issues with Lubridate interval columns)
      mutate(PSQI_01_calc = lubridate::hms(PSQI_01),
             PSQI_03_calc = lubridate::hms(PSQI_03)) %>% 
      
      # Remove unecessary columns
      select(-c01,-c03) %>% 
      
      # Make sure certain variables are numeric
      mutate_at(vars(matches("PSQI_0[5-9]$|PSQI_0[5-9][a-j]$|PSQI_1[0-1]$|PSQI_1[0-1][a-j]$")),as.integer) 
  )
  
  # Do some minor adjustments, and initial calculations
  df = df %>% 
    mutate(
      
      # Scale Q2
      PSQI_02_scaled_calc = ifelse(PSQI_02 >= 15 & PSQI_02 <= 30, 1, 
                                   ifelse(PSQI_02 >= 31 & PSQI_02 <= 60,2,
                                          ifelse(PSQI_02 >= 60, 3, 0))),
      
      # Works around varying punching of after midnight Bedtime_calcs (added 24hr for easy calculations)
      Bedtime_calc = if_else(hour(PSQI_01_calc)<=5, PSQI_01_calc+hours(24), PSQI_01_calc),
      
      
      # Adds 24hr to rising time, for easy calculation of time spent in bed
      Risingtime_calc = PSQI_03_calc+hours(24),
      
      # Calculate time spend in bed from Bedtime_calc and rising time
      TimeInBed_calc = (Risingtime_calc-Bedtime_calc) %>% 
        as.period(unit = "hours") %>% 
        hms2deciH(),
      
      # If Hours of sleep (Q4) is not answered, calculate it by TimeInBed_calc-minutes to sleep.
      PSQI_04_calc = ifelse(!is.na(PSQI_04), PSQI_04, 
                            ifelse(!is.na(PSQI_02), TimeInBed_calc-(PSQI_02/60),
                                   TimeInBed_calc))
      
    )
  
  
  
  #### --- Calculate components --- ####
  df = df %>% 
    mutate(
      
      # Component 1 is assessment of sleep quality in Q6
      dPSQI_Comp1_SleepQuality     = PSQI_06, 
      
      # Component 2 is a latency score, the sum of scaled Q2 with Q5a, and then scaled
      PSQI_Comp2_sum_calc = PSQI_02_scaled_calc + PSQI_05a,
      PSQI_Comp2_Latency = ifelse(PSQI_Comp2_sum_calc >= 1 & PSQI_Comp2_sum_calc <= 2, 1,
                                  ifelse(PSQI_Comp2_sum_calc >= 3 & PSQI_Comp2_sum_calc <= 4, 2, 
                                         ifelse(PSQI_Comp2_sum_calc >=5, 3, 0))),
      
      # Component 3 is a scaling of sleep duration, if q4 is NA, calculate sleep by TimeInBed_calc-minutes to sleep
      PSQI_Comp3_Duration = ifelse(PSQI_04_calc > 7, 0,
                                   ifelse(PSQI_04_calc >= 6 & PSQI_04_calc <= 7, 1,
                                          ifelse(PSQI_04_calc >= 5 & PSQI_04_calc < 6, 2, 3))),
      
      # Component 4 is a scaled indicator of sleep efficiency
      PSQI_Comp4_Percent_calc = (PSQI_04_calc/TimeInBed_calc)*100,
      PSQI_Comp4_Efficiency = ifelse(PSQI_Comp4_Percent_calc > 85, 0,
                                     ifelse(PSQI_Comp4_Percent_calc <= 85 & PSQI_Comp4_Percent_calc >= 75, 1,
                                            ifelse(PSQI_Comp4_Percent_calc < 75 & PSQI_Comp4_Percent_calc >= 65, 2, 3))),
      
      # Get all the 5-questions that are not strings. This assumes all the Q5's column names end
      # with "_5" and the characters a-j in small case
      PSQI_Comp_5_sum_calc = ifelse(!is.na(PSQI_05a),rowSums(select(.,matches("_05[a-j]$")), na.rm=T), NA),
      PSQI_Comp5_Problems = ifelse(PSQI_Comp_5_sum_calc == 0, 0,
                                   ifelse(PSQI_Comp_5_sum_calc >= 1 & PSQI_Comp_5_sum_calc < 10, 1,
                                          ifelse(PSQI_Comp_5_sum_calc >= 10 & PSQI_Comp_5_sum_calc < 19, 2, 3))),
      
      PSQI_Comp6_Medication = PSQI_07,
      
      PSQI_Comp7_sum_calc = rowMeans(cbind(df$PSQI_08,df$PSQI_09), na.rm = T)*2,
      PSQI_Comp7_Tired = ifelse(PSQI_Comp7_sum_calc == 0, 0,
                                ifelse(PSQI_Comp7_sum_calc > 0 & PSQI_Comp7_sum_calc <= 2, 1,
                                       ifelse(PSQI_Comp7_sum_calc > 2 & PSQI_Comp7_sum_calc <= 4, 2, 3)))
    )
  
  tmp = df %>% select(ends_with("_calc"))
  
  df = df %>% 
    select(-ends_with("_calc")) %>% 
    mutate(
      PSQI_Global = rowSums(select(.,contains("PSQI_Comp")), na.rm = T),
      
      # If Global is missing at least 4 values, set it to NA.
      PSQI_Global = ifelse(rowSums(is.na(select(.,contains("PSQI_Comp")))) > 4 , NA, PSQI_Global)
    )
  
  # If you want to keep the values used for the Component calculations, add them again here.
  if(keep.intermediate) df = df %>% cbind.data.frame(tmp)
  
  # Return the entire incoming df with the new values added
  df
}
