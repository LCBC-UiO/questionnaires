#' Alter the time instant columns to decminals
#' 
#' Time is often punched as HH:MM in order
#' to preserve correct time calculations.
#' The ipaq calculation recure time to be
#' in decimal minutes. 
#' This function easily changes HH:MM into
#' decminal minutes in a data.frame
#' It alters columns directly in the data.frame
#' 
#' @param data data with columns to alter
#' @param cols columns to alter, in tidyselect format
#' @return data.frame
#' @family ipaq_functions
#' @export
#' @importFrom dplyr mutate across
#' @examples 
#' dat <- data.frame(
#'    time_1  = c("12:34", "09:33", "22:14"),
#'    time_2  = c("10:55", "16:45", "18:02")
#' )
#' ipaq_time_alter(dat, cols = c(time_1, time_2))
ipaq_time_alter <- function(data, cols = c(ipaq_2, ipaq_4, ipaq_6, ipaq_7)){
  mutate(data, across( {{cols}} , 
                       ~time_alter(.x, unit = "minute"))
  )
  
}

#' IPAQ mets
#' 
#' IPAQ calculations require specification of 
#' met (resting metabolic rate), which are not
#' necessarily static values. While there are defaults
#' for each of the three categories, there should
#' be the possibility to alter these with newer research.
#' 
#' This is a convenience function if users need to
#' alter the default values for one or more of the categories
#' and is compatible with the remaining IPAQ functions
#' in this package.
#'
#' @param light numeric. default 3.3
#' @param moderate numeric. default 4.0
#' @param vigorous numeric. default 8.0
#'
#' @return list of three
#' @export
#' @family ipaq_functions
#' @examples
#' ipaq_mets()
#' ipaq_mets(moderate = 5.1)
ipaq_mets <- function(light = 3.3, moderate = 4.0, vigorous = 8.0){
  list(
    light = light, 
    moderate = moderate,
    vigorous = vigorous
  )
}

#' Compute met from IPAQ
#' @param minutes vector of numeric minutes
#' @param days vector of numeric days
#' @param met met number (light = 3.3, moderate = 4.0, vigorous = 8)
#' @export
#' @describeIn ipaq_compute Calculate mets of an activity type
#' @examples 
#' ipaq_vig_mins <- c(60, 20, 60, 25, 90, 20, 0, 75, 60, 30)
#' ipaq_vig_days <- c(1, 3, 2, 5, 6, 1, 1, 2, 2, 4)
#' ipaq_compute_met(ipaq_vig_mins, ipaq_vig_days, met = 8.0)
ipaq_compute_met <- function(minutes = ipaq_2,
                             days = ipaq_1b,
                             met = ipaq_mets()$light ){
  minutes * days * met
}


#' Calculate the IPAQ sum
#' @param vigorous Vector with vigorous met calculated
#' @param moderate Vector with moderate met calculated
#' @param light Vector with light met calculated
#' @export
#' @describeIn ipaq_compute Calculate the IPAQ sum based on activities and mets
#' @examples
#' light = c(1300, 300)
#' moderate = c(200, 400)
#' vigorous = c(0, 1300)
#' ipaq_compute_sum(vigorous , moderate, light)
ipaq_compute_sum <- function(vigorous, moderate, light){
  tmp <- data.frame(vigorous, moderate, light)
  rowSums(tmp, na.rm=T)
}


#' Compute all the IPAQ components
#' 
#' ```{r child="man/fragments/ipaq/background.Rmd"}
#' ```
#' ## Scoring
#' ```{r child="man/fragments/ipaq/scoring.Rmd"}
#' ```
#' ## References
#' ```{r child="man/fragments/gds/references.Rmd"}
#' ```
#' 
#' @param data data.frame containing all the ipaq data
#' @param mets list generated with ipaq_mets() (default = ipaq_mets())
#' @param light_days column with the days of light activity
#' @param light_mins column with the minutes of light activity
#' @param mod_days column with the days of moderate activity
#' @param mod_mins column with the minutes of moderate activity
#' @param vig_days column with the days of vigorous activity
#' @param vig_mins column with the minutes of vigorous activity
#' @template keep_all
#' @template prefix
#' @family ipaq_functions
#' @return data.frame
#' @export
#' @importFrom dplyr transmute mutate bind_cols select one_of contains case_when
#' @importFrom dplyr select across everything pull rowwise c_across
ipaq_compute <- function(data, 
                         mets = ipaq_mets(),
                         light_days = ipaq_5b,
                         light_mins = ipaq_6,
                         mod_days = ipaq_3b,
                         mod_mins = ipaq_4,
                         vig_days = ipaq_1b,
                         vig_mins = ipaq_2,
                         prefix = "ipaq_",
                         keep_all = TRUE){
  tmp <- rowwise(data)
  tmp <- transmute(tmp,
                   .valid1 = sum(c_across(c({{light_mins}}, 
                                            {{mod_mins}}, 
                                            {{vig_mins}})), 
                                 na.rm = TRUE),
                   .valid2 = sum(c_across(c({{light_mins}}, 
                                            {{mod_mins}})), 
                                 na.rm = TRUE),
                   met_vigorous = ipaq_compute_met(minutes = {{vig_mins}}, 
                                                   days = {{vig_days}}, 
                                                   mets$vigorous),
                   met_moderate = ipaq_compute_met(minutes = {{mod_mins}}, 
                                                   days = {{mod_days}}, 
                                                   mets$moderate),
                   met_light = ipaq_compute_met(minutes = {{light_mins}}, 
                                                days = {{light_days}}, 
                                                mets$light),
                   met = ipaq_compute_sum(met_vigorous, 
                                          met_moderate, 
                                          met_light),
                   {{light_mins}}, {{light_days}},
                   {{mod_mins}}, {{mod_days}},
                   {{vig_mins}}, {{vig_days}}
  )
  tmp <- mutate(tmp, 
                across(everything(), zero2na),
                coded = case_when( 
                  (met_vigorous >= 1500 & {{vig_days}} >= 3) | 
                    (.valid1 >= 7 & met >= 3000) ~ "high",
                  ( {{vig_mins}} >= 20 & {{vig_days}} >= 3) | 
                    (.valid2 >= 5 & ( {{light_days}} >= 30 | {{mod_days}} >= 30) ) ~ "moderate",
                  TRUE ~ "low"))
  tmp <- select(tmp,            
                -.valid1, -.valid2,
                -{{light_mins}}, -{{light_days}},
                -{{mod_mins}}, -{{mod_days}},
                -{{vig_mins}}, -{{vig_days}})
  
  if(!is.null(prefix)){
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
  }
  
  if(keep_all){
   tmp <- bind_cols(data, tmp)
  }
   tmp
}

if(getRversion() >= "2.15.1") 
  utils::globalVariables(c("ipaq_1b", "ipaq_2", "ipaq_3b", "ipaq_4", 
                          "ipaq_5b", "ipaq_6", "ipaq_7",
                          "met_light", "ipaq_met", "met_moderate", 
                          ".valid1", ".valid2",
                          "met_vigorous", "ipaq_coded"))
