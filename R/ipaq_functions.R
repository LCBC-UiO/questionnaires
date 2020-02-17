#' Alter the time instant columns to decminals
#' 
#' Time is often punched as HH:MM in order
#' to preserve correct time calculations.
#' The IPAQ calculation recure time to be
#' in decimal minutes. 
#' This function easily changes HH:MM into
#' decminal minutes in a data.frame
#' It alters columns directly in the data.frame
#' 
#' @param data data with columns to alter
#' @param cols columns to alter
#' @return data.frame
#' @family ipaq_functions
#' @export
#' @importFrom dplyr enquo mutate_at vars funs
#' @examples 
#' dat <- data.frame(
#'    time_1  = c("12:34", "09:33", "22:14"),
#'    time_2  = c("10:55", "16:45", "18:02")
#' )
#' ipaq_time_alter(dat, cols = c(time_1, time_2))
ipaq_time_alter <- function(data, cols = c(IPAQ_2, IPAQ_4, IPAQ_6)){
  mutate_at(data, vars( {{cols}} ), 
            function(x) time_alter(x, unit = "minute"))
  
}

#' IPAQ mets
#' 
#' IPAQ calculations require specification of 
#' MET (resting metabolic rate), which are not
#' necessarily static values. While there are defaults
#' for each of the three categories, there should
#' be the possibility to alter these with newer research.
#' 
#' This is a convenience function if users need to
#' alter the default values for one or more of the categories
#' and is compliable with the remaining ipaw functions
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

#' Compute MET from IPAQ
#'
#' To calculate the MET score for a met category,
#' the minutes spent and the number of days of this 
#' activity needs to be multiplied with the met-value 
#' of the activity. This function allows you to specify
#' a vector with the minues, another with the days, and
#' lastly a value to use as the MET of that activity level.
#'
#' @param minutes vector of numeric minutes
#' @param days vector of numeric days
#' @param met MET number (light = 3.3, moderate = 4.0, vigorous = 8)
#' @family ipaq_functions
#' @return numeric vector
#' @export
#' @examples 
#' ipaq_vig_mins <- c(60, 20, 60, 25, 90, 20, 0, 75, 60, 30)
#' ipaq_vig_days <- c(1, 3, 2, 5, 6, 1, 1, 2, 2, 4)
#' ipaq_compute_met(ipaq_vig_mins, ipaq_vig_days, met = 8.0)
ipaq_compute_met <- function(minutes = IPAQ_2,
                             days = IPAQ_1b,
                             met = ipaq_mets()$light ){
  minutes * days * met
  
}


#' Calculate the IPAQ sum
#' 
#' The IPAQ sum is based on the calculated MET-scores
#' for the three categories of met activity.
#' This function takes three arguments, which are
#' the pre-calulcated MET scores for each category.
#' 
#' @param vigorous Vector with vigorous MET calculated
#' @param moderate Vector with moderate MET calculated
#' @param light Vector with light MET calculated
#' @export
#' @family ipaq_functions
#' @return numeric vector
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
#' In most cases you have all the IPAQ
#' data in a data frame and would like to
#' calculate and add all the data directly
#' to your data frame.
#' 
#' This function takes many arguments in order to
#' do that.
#' 
#' @param data data.frame containing all the IPAQ data
#' @param mets list generated with ipaq_mets() [default = ipaq_mets()]
#' @param light_days column with the days of light activity
#' @param light_mins column with the minutes of light activity
#' @param mod_days column with the days of moderate activity
#' @param mod_mins column with the minutes of moderate activity
#' @param vig_days column with the days of vigorous activity
#' @param vig_mins column with the minutes of vigorous activity
#' @param keep_all logical, append to data.frame
#' @family ipaq_functions
#' @return data.frame
#' @importFrom dplyr transmute mutate mutate_all bind_cols select one_of contains case_when
ipaq_compute <- function(data, 
                         mets = ipaq_mets(),
                         light_days = IPAQ_1b,
                         light_mins = IPAQ_2,
                         mod_days = IPAQ_3b,
                         mod_mins = IPAQ_4,
                         vig_days = IPAQ_5b,
                         vig_mins = IPAQ_6,
                         keep_all = TRUE){
  
  tmp <- dplyr::transmute(data,
                          IPAQ_MET_Vigorous = ipaq_compute_met(minutes = {{vig_mins}}, 
                                                               days = {{vig_days}}, 
                                                               mets$vigorous),
                          IPAQ_MET_Moderate = ipaq_compute_met(minutes = {{mod_mins}}, 
                                                               days = {{mod_days}}, 
                                                               mets$moderate),
                          IPAQ_MET_Light = ipaq_compute_met(minutes = {{light_mins}}, 
                                                            days = {{light_days}}, 
                                                            mets$light)
  )
  
  tmp <- dplyr::mutate(tmp, 
                       IPAQ_MET = ipaq_compute_sum(IPAQ_MET_Vigorous, 
                                                   IPAQ_MET_Moderate, 
                                                   IPAQ_MET_Light))
  tmp <- dplyr::mutate_all(tmp, zero2na)
  
  tmp <- dplyr::bind_cols(dplyr::select(data, 
                                        -dplyr::one_of(names(data)[names(data) %in% names(tmp)])), 
                          tmp)
  
  # Create a some validator vectors, used for the MET factorial
  valid1 <- dplyr::select(data, 
                          {{light_mins}}, {{mod_mins}}, {{vig_mins}} )
  valid1 <- rowSums(valid1)
  
  valid2 <- dplyr::select(data, 
                          {{light_mins}}, {{mod_mins}} )
  valid2 <- rowSums(valid2)
  
  q5 <- unlist(dplyr::select(data, {{vig_mins}} ))
  q3 <- unlist(dplyr::select(data, {{mod_mins}} ))
  q1 <- unlist(dplyr::select(data, {{light_mins}} ))
  q6 <- unlist(dplyr::select(data, {{vig_days}} ))
  
  tmp <- dplyr::mutate(tmp, 
                       IPAQ_Coded = dplyr::case_when( 
                         (IPAQ_MET_Vigorous >= 1500 & {{vig_days}} >= 3) | 
                           (valid1 >= 7 & IPAQ_MET >= 3000) ~ "High",
                         ( {{vig_mins}} >= 20 & {{vig_days}} >= 3) | 
                           (valid2 >= 5 & ( {{light_days}} >= 30 | {{mod_days}} >= 30) ) ~ "Moderate",
                         TRUE ~ "Low"))

if(keep_all){
  tmp
}else{
  dplyr::select(tmp,
                dplyr::contains("IPAQ_MET"),
                IPAQ_Coded)
}

}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("IPAQ_1b", "IPAQ_2", "IPAQ_3b", "IPAQ_4", 
                                                        "IPAQ_5b", "IPAQ_6",
                                                        "IPAQ_MET_Light", "IPAQ_MET", "IPAQ_MET_Moderate", 
                                                        "IPAQ_MET_Vigorous", "IPAQ_Coded"))
