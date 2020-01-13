
#' Compute time in bed
#' @param bedtime column name with bedtime (HH:MM:SS) [PSQI_01]
#' @param risingtime column name with rising time (HH:MM:SS) [PSQI_03]
#' @importFrom dplyr if_else
#' @importFrom lubridate hms period_to_seconds hours
compute_time_in_bed <- function(risingtime, bedtime){
  tmp <- hms(risingtime, quiet = T) - hms(bedtime, quiet = T)
  period_to_seconds(if_else(as.numeric(tmp) < 0, hours(24) + tmp, tmp))/3600
}

#' Compute component PSQI 2
#' 
#' Computes the second PSQI component on sleep quality, 
#' using the number of minutes before sleep (PSQI Q2) and
#' subjective evaluation of sleep within 30 minutes
#' 
#' @param minBeforeSleep column name with no. minutes before sleep (numeric) [PSQI_02]
#' @param noSleep30min column name with evaluation of sleep within 30min (0-3) [PSQI_05a]
#' @family psqi_functions
#' @export
psqi_compute_comp2 <- function(minBeforeSleep, noSleep30min){
  
  tmp <- cut(minBeforeSleep, breaks = c(0, 15, 31, 61, Inf), 
             labels = FALSE, include.lowest = TRUE, right = FALSE) - 1L
  
  cut(tmp + noSleep30min, breaks = c(0, 1, 3, 5, Inf),
      labels = FALSE, include.lowest = TRUE, right = FALSE) - 1L
  
}

#' Compute component PSQI 3
#' 
#' @param hoursSleep column name with hours of sleep (decimal hours) [PSQI_04]
#' @family psqi_functions
#' @export
psqi_compute_comp3 <- function(hoursSleep){
  4L - cut(hoursSleep, breaks = c(0, 4.999, 5.999, 7, Inf), 
           labels = FALSE, include.lowest = TRUE, right = TRUE)
}

#' Compute component PSQI 4
#' 
#' @param bedtime column name with bedtime (HH:MM:SS) [PSQI_01]
#' @param risingtime column name with rising time (HH:MM:SS) [PSQI_03]
#' @param hoursSleep column name with hours of sleep (decimal hours) [PSQI_04]
#' @family psqi_functions
#' @export
psqi_compute_comp4 <- function(hoursSleep, bedtime, risingtime){
  4L - cut(hoursSleep / compute_time_in_bed(risingtime, bedtime) * 100,
           breaks = c(0, 65, 75, 85.0001, Inf), labels = FALSE, include.lowest = TRUE,
           right = FALSE)
}

#' Compute component PSQI 5
#' 
#' @param data data frame with the data
#' @param sleepTroubles columns containing sleep problem evaluations (0-3) [PSQI_05[b-j] ]
#' @family psqi_functions
#' @export
#' @importFrom dplyr enquo select mutate if_else pull
psqi_compute_comp5 <- function(data, sleepTroubles = matches("^PSQI_05[b-j]$")){

  tmp <- select(data, {{sleepTroubles}} )
  tmp <- mutate(tmp, value = apply(tmp, 1, function(x) 
    if_else(all(is.na(x)), NA_real_, as.numeric(sum(x, na.rm = TRUE))))
    )
  tmp <- pull(tmp, value)
  
  cut(tmp, breaks = c(0, 1, 10, 19, Inf),
      labels = FALSE, include.lowest = TRUE, right = FALSE) - 1L
}

#' Calculate component 7 of PSQI
#' 
#' @param keepAwake column name with evaluation of staying awake (0-3) [PSQI_08]
#' @param keepEnthused column name with evaluation of keeping enthusiastic (0-3) [PSQI_09]
#' @family psqi_functions
#' @export
psqi_compute_comp7 <- function(keepAwake, keepEnthused){
  cut(keepAwake + keepEnthused,
      breaks = c(-Inf, 0, 2, 4, Inf), labels = FALSE, include.lowest = TRUE, 
      right = TRUE) - 1L
}

#' Compute Global PSQI component
#' 
#' @param data Data frame containing PSQI components
#' @param cols columns containing the components
#' @family psqi_functions
#' @inheritParams psqi_compute
#' @export
#' @importFrom dplyr enquo select group_by_at n row_number summarise_at pull if_else
#' @importFrom tidyr gather
psqi_compute_global <- function(data, cols = matches("^PSQI_Comp[1-7]+_"), max_missing = 0){
  if(max_missing > 6) stop("max_missing must be between 0 and 6.", call. = FALSE)

  tmp <- select(data, {{cols}} )
  tmp <- mutate(tmp, ID = row_number())
  tmp <- gather(tmp, key = "key", value = "value", {{cols}} , na.rm = FALSE)
  
  tmp <- group_by_at(tmp, vars(ID))

  tmp <- summarise_at(tmp, vars(value), 
                      list(~ if_else(7 - sum(!is.na(.)) > max_missing, 
                                     NA_real_, 
                                     7 / sum(!is.na(.)) * sum(., na.rm = TRUE))))
  pull(tmp)
}

#' Compute all PSQI components and global score
#' 
#' @param data data frame
#'
#' @param components integer vector of components to calculate. If all 7, global is added also
#' @param bedtime column name with bedtime (HH:MM:SS) [PSQI_01]
#' @param minBeforeSleep column name with no. minutes before sleep (numeric) [PSQI_02]
#' @param risingtime column name with rising time (HH:MM:SS) [PSQI_03]
#' @param hoursSleep column name with hours of sleep (decimal hours) [PSQI_04]
#' @param noSleep30min column name with evaluation of sleep within 30min (0-3) [PSQI_05a]
#' @param sleepQuality column name with evaluation of sleep quality (0-3) [PSQI_06]
#' @param medication column name with use of sleep mediation (0-3) [PSQI_07]
#' @param keepAwake column name with evaluation of staying awake (0-3) [PSQI_08]
#' @param keepEnthused column name with evaluation of keeping enthusiastic (0-3) [PSQI_09]
#' @param sleepTroubles columns containing sleep problem evaluations (0-3) [PSQI_05[a-j] ]
#' @param max_missing Integer specifying the number of missing values to accept in the PSQI components, 
#' before the global PSQI value is set to missing. Defaults to 0. If \code{max_missing > 0}, the 
#' global PSQI value is computed by weighting each non-missing entry with \code{7 / (7 - max_missing)}.
#' @param keep_all logical, append to data.frame
#'
#' @return a data.frame containing only the calculated components
#' @export
#' @family psqi_functions
#' @importFrom dplyr enquo mutate matches
psqi_compute <- function(data, 
                         components = 1:7,
                         bedtime = PSQI_01, 
                         minBeforeSleep = PSQI_02, 
                         risingtime = PSQI_03, 
                         hoursSleep = PSQI_04, 
                         noSleep30min = PSQI_05a, 
                         sleepQuality = PSQI_06, 
                         medication = PSQI_07, 
                         keepAwake = PSQI_08, 
                         keepEnthused = PSQI_09,
                         sleepTroubles = matches("^PSQI_05[b-j]$"), 
                         max_missing = 0,
                         keep_all = TRUE
){
  
  tmp <- data
  nn <- character()

  if(1 %in% components){
    tmp <- mutate(tmp, PSQI_Comp1_Quality = {{sleepQuality}} )
    nn = c(nn, "PSQI_Comp1_Quality")
  }
  
  if(2 %in% components){
    tmp <- mutate(tmp, 
                  PSQI_Comp2_Latency = psqi_compute_comp2({{minBeforeSleep}} , 
                                                               {{noSleep30min}} ))
    nn = c(nn, "PSQI_Comp2_Latency")  
  } 
  
  if(3 %in% components){
    tmp <- mutate(tmp, 
                  PSQI_Comp3_Duration = psqi_compute_comp3({{hoursSleep}} ))
    nn = c(nn, "PSQI_Comp3_Duration")
  } 
  
  if(4 %in% components){
    tmp <- mutate(tmp, 
                  PSQI_Comp4_Efficiency = psqi_compute_comp4({{hoursSleep}} , 
                                                                  {{bedtime}},  
                                                                  {{risingtime}} ))
    nn = c(nn, "PSQI_Comp4_Efficiency")
  } 
  
  if(5 %in% components){
    tmp <- mutate(tmp, 
                  PSQI_Comp5_Problems = psqi_compute_comp5(data, sleepTroubles))
    nn = c(nn, "PSQI_Comp5_Problems")
  } 
  
  if(6 %in% components){
    tmp <- mutate(tmp, 
                  PSQI_Comp6_Medication = {{medication}} )
    nn = c(nn, "PSQI_Comp6_Medication")
  } 
  
  if(7 %in% components){
    tmp <- mutate(tmp, 
                  PSQI_Comp7_Tired = psqi_compute_comp7({{keepAwake}} , {{keepEnthused}} ))
    nn = c(nn, "PSQI_Comp7_Tired")
  } 
  
  if(!keep_all) tmp <- select(tmp, {{nn}} )
  
  if(all(1:7 %in% components)){
    mutate(tmp,
           PSQI_Global = psqi_compute_global(tmp, max_missing = max_missing)
    )
  }else{
    tmp
  }
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("PSQI_01", "PSQI_02", "PSQI_03", "PSQI_04", 
                                                        "PSQI_05a", "PSQI_06", "PSQI_07", "PSQI_08", "PSQI_09",
                                                        "value", "ID"))

