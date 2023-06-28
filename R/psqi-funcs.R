
#' PSQI compute time in bed
#' @param bedtime column name with bedtime (HH:MM:SS) (psqi_01)
#' @param risingtime column name with rising time (HH:MM:SS) (psqi_03)
#' @param risingtime_func function to convert time to \code{Period}
#' @param bedtime_func function to convert time to \code{Period}
#' @importFrom dplyr if_else
#' @importFrom lubridate hms period_to_seconds hours
#' @export
psqi_compute_time_in_bed <- function(risingtime, bedtime, 
                                risingtime_func = lubridate::hm,
                                bedtime_func = lubridate::hm){
  tmp <- risingtime_func(
    risingtime, quiet = TRUE) - 
    bedtime_func(bedtime, quiet = TRUE)
  period_to_seconds(if_else(as.numeric(tmp) < 0, hours(24) + tmp, tmp))/3600
}

#' @param min_before_sleep column name with no. minutes before sleep (numeric) (psqi_02)
#' @param no_sleep_30min column name with evaluation of sleep within 30min (0-3) (psqi_05a)
#' @describeIn psqi_compute calculate the component 2 (sleep latency)
#' @export
psqi_compute_comp2 <- function(min_before_sleep, no_sleep_30min){
  
  tmp <- cut(min_before_sleep, breaks = c(0, 15, 31, 61, Inf), 
             labels = FALSE, include.lowest = TRUE, right = FALSE) - 1L
  
  cut(tmp + no_sleep_30min, breaks = c(0, 1, 3, 5, Inf),
      labels = FALSE, include.lowest = TRUE, right = FALSE) - 1L
  
}

#' @param hours_sleep column name with hours of sleep (decimal hours) (psqi_04)
#' @describeIn psqi_compute calculate the component 3 (sleep duratione)
#' @export
psqi_compute_comp3 <- function(hours_sleep){
  4L - cut(hours_sleep, breaks = c(0, 4.999, 5.999, 7, Inf), 
           labels = FALSE, include.lowest = TRUE, right = TRUE)
}

#' @param bedtime column name with bedtime (HH:MM:SS) (psqi_01)
#' @param risingtime column name with rising time (HH:MM:SS) (psqi_03)
#' @param ... other arguments to \code{\link{psqi_compute_time_in_bed}}
#' @describeIn psqi_compute calculate the component 4 (habitual sleep efficiency)
#' @export
psqi_compute_comp4 <- function(hours_sleep, bedtime, risingtime, ...){
  4L - cut(hours_sleep / psqi_compute_time_in_bed(risingtime, bedtime, ...) * 100,
           breaks = c(0, 65, 75, 85.0001, Inf), labels = FALSE, include.lowest = TRUE,
           right = FALSE)
}

#' @param data data frame with the data
#' @param sleep_troubles columns containing sleep problem evaluations (0-3) (psqi_05(b-j) )
#' @describeIn psqi_compute calculate the component 5 (sleep disturbance)
#' @export
#' @importFrom dplyr select mutate if_else pull
psqi_compute_comp5 <- function(data, sleep_troubles = matches("^psqi_05[b-j]$")){
  tmp <- select(data,  
                {{sleep_troubles}})
  tmp <- mutate(tmp,
                value = apply(tmp, 1, function(x) 
                  if_else(all(is.na(x)), NA_real_, as.numeric(sum(x, na.rm = TRUE))))
  )
  tmp <- pull(tmp, value)
  
  cut(tmp, breaks = c(0, 1, 10, 19, Inf),
      labels = FALSE, include.lowest = TRUE, right = FALSE) - 1L
}

#' @param keep_awake column name with evaluation of staying awake (0-3) (psqi_08)
#' @param keep_enthused column name with evaluation of keeping enthusiastic (0-3) (psqi_09)
#' @describeIn psqi_compute calculate the component 7 (daytime dysfunction)
#' @export
psqi_compute_comp7 <- function(keep_awake, keep_enthused){
  cut(keep_awake + keep_enthused,
      breaks = c(-Inf, 0, 2, 4, Inf), labels = FALSE, include.lowest = TRUE, 
      right = TRUE) - 1L
}

#' @param data Data frame containing PSQI components
#' @param cols columns containing the components
#' @family psqi_functions
#' @export
#' @describeIn psqi_compute calculate the global scores, sum of all components
#' @importFrom dplyr summarise if_else select group_by
#' @importFrom tidyr gather
psqi_compute_global <- function(data, cols = matches("comp[1-7]+_"), max_missing = 0){
  if(max_missing > 6) 
    cli::cli_abort("max_missing must be between 0 and 6.")
  tmp <- select(data, {{cols}})
  tmp <- mutate(tmp, .r = 1:nrow(data))
  tmp <- gather(tmp, 
                key = "key", 
                value = "value", 
                {{cols}}, na.rm = FALSE)
  tmp <- group_by(tmp, .r)
  tmp <- summarise(tmp, 
            .n = sum(!is.na(value)),
            value = if_else(7 - .n > max_missing,
                            NA_real_,
                            7/.n * sum(value, na.rm = TRUE))
            )
  tmp$value
}

#' Compute all PSQI components and global score
#' 
#' ```{r child="man/fragments/psqi/background.Rmd"}
#' ```
#' ##Scoring
#' ```{r child="man/fragments/psqi/scoring.Rmd"}
#' ```
#' ## Data requirements
#' ```{r child="man/fragments/psqi/datareq.Rmd"}
#' ```
#' ## References
#' ```{r child="man/fragments/psqi/references.Rmd"}
#' ```
#' 
#' @param data data frame
#' @param components integer vector of components to calculate. If all 7, global is added also
#' @param sleepquality column name with evaluation of sleep quality (0-3) (psqi_06)
#' @param medication column name with use of sleep mediation (0-3) (psqi_07)
#' @param max_missing Integer specifying the number of missing values to accept in the PSQI components, 
#' before the global PSQI value is set to missing. Defaults to 0. If \code{max_missing > 0}, the 
#' global PSQI value is computed by weighting each non-missing entry with \code{7 / (7 - max_missing)}.
#' @template keep_all
#' @template prefix
#' @param ... other arguments to \code{\link{psqi_compute_time_in_bed}}
#'
#' @return a data.frame containing only the calculated components
#' @export
#' @family psqi_functions
#' @importFrom dplyr mutate matches
psqi_compute <- function(data, 
                         components = 1:7,
                         bedtime = psqi_01, 
                         min_before_sleep = psqi_02, 
                         risingtime = psqi_03, 
                         hours_sleep = psqi_04, 
                         no_sleep_30min = psqi_05a, 
                         sleepquality = psqi_06, 
                         medication = psqi_07, 
                         keep_awake = psqi_08, 
                         keep_enthused = psqi_09,
                         sleep_troubles = matches("^psqi_05[b-j]$"), 
                         max_missing = 0,
                         ...,
                         prefix = "psqi_",
                         keep_all = TRUE
){
  
  tmp <- select(data, {{bedtime}}, {{min_before_sleep}},
                {{risingtime}}, {{hours_sleep}},
                {{no_sleep_30min}}, {{sleepquality}},
                {{medication}}, {{keep_awake}},
                {{keep_enthused}}, {{sleep_troubles}})
  nn <- character()
  
  if(1 %in% components){
    tmp <- mutate(tmp, comp1_quality = {{sleepquality}} )
    nn = c(nn, "comp1_quality")
  }
  
  if(2 %in% components){
    tmp <- mutate(tmp, 
                  comp2_latency = psqi_compute_comp2({{min_before_sleep}} , 
                                                     {{no_sleep_30min}} ))
    nn = c(nn, "comp2_latency")  
  } 
  
  if(3 %in% components){
    tmp <- mutate(tmp, 
                  comp3_duration = psqi_compute_comp3({{hours_sleep}} ))
    nn = c(nn, "comp3_duration")
  } 
  
  if(4 %in% components){
    tmp <- mutate(tmp, 
                  comp4_efficiency = psqi_compute_comp4({{hours_sleep}} , 
                                                        {{bedtime}},  
                                                        {{risingtime}},
                                                        ...))
    nn = c(nn, "comp4_efficiency")
  } 
  
  if(5 %in% components){
    tmp <- mutate(tmp, 
                  comp5_problems = psqi_compute_comp5(data,
                                                      {{sleep_troubles}}))
    nn = c(nn, "comp5_problems")
  } 
  
  if(6 %in% components){
    tmp <- mutate(tmp, 
                  comp6_medication = {{medication}} )
    nn = c(nn, "comp6_medication")
  } 
  
  if(7 %in% components){
    tmp <- mutate(tmp, 
                  comp7_tired = psqi_compute_comp7({{keep_awake}} , {{keep_enthused}} ))
    nn = c(nn, "comp7_tired")
  } 
  
  
  if(all(1:7 %in% components)){
    tmp <- mutate(tmp,
                  global = psqi_compute_global(tmp, max_missing = max_missing)
    )
    nn = c(nn, "global")
  }
  
  if(!is.null(prefix)){
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
    nn <- paste0(prefix, nn)
  }
  
  tmp <- select(tmp, {{nn}} )
  
  if(keep_all) 
    tmp <- bind_cols(data, tmp)
  
  tmp
}

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("psqi_01", "psqi_02", "psqi_03", "psqi_04", 
                           "psqi_05a", "psqi_06", "psqi_07", "psqi_08", "psqi_09",
                           "value", ".r", ".n"))

