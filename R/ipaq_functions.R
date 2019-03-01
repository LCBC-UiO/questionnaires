
#' @importFrom dplyr enquo mutate_at vars funs
ipaq_time_alter <- function(data, cols = c(IPAQ_2, IPAQ_4, IPAQ_6, IPAQ_7)){
  cols <- enquo(cols)
  
  fn <- function(x) as.numeric(time_alter(x, type="minute"))
  mutate_at(data, vars(!!cols), fn)
  
}

#' Compute MET from IPAQ
#'
#' @param minutes vector of numeric minutes
#' @param days vector of numeric days
#' @param met MET number (light = 3.3, moderate = 4.0, vigurous = 8)
#'
#' @return
#' @export
#'
#' @examples
ipaq_compute_met <- function(minutes = IPAQ_2,
                             days = IPAQ_1b,
                             met = 3.3){
  minutes*days*met
  
}


#'

#' @importFrom dplyr enquo select
ipaq_compute_sum <- function(vigurous, moderate, light){
  vigurous + moderate + moderate
}


#' @param data 
#'
#' @param mets 
#' @param light_days 
#' @param light_mins 
#' @param mod_days 
#' @param mod_mins 
#' @param vig_days 
#' @param vig_mins 
#' @param keep_all 
#'
#' @importFrom dplyr enquo transmute mutate mutate_all bind_cols select one_of contains
ipaq_compute <- function(data, 
                         mets = list(light = 3.3, moderate = 4.0, vigorous = 8.0),
                         light_days = IPAQ_1b,
                         light_mins = IPAQ_2,
                         mod_days = IPAQ_3b,
                         mod_mins = IPAQ_4,
                         vig_days = IPAQ_5b,
                         vig_mins = IPAQ_6,
                         keep_all = TRUE){
  
  light_mins <- dplyr::enquo(light_mins)
  light_days <- dplyr::enquo(light_days)
  
  mod_mins <- dplyr::enquo(mod_mins)
  mod_days <- dplyr::enquo(mod_days)
  
  vig_mins <- dplyr::enquo(vig_mins)
  vig_days <- dplyr::enquo(vig_days)
  
  tmp <- dplyr::transmute(data,
                   IPAQ_MET_Vigorous = ipaq_compute_met(minutes = !!vig_mins, days = !!vig_days, mets$vigorous),
                   IPAQ_MET_Moderate = ipaq_compute_met(minutes = !!mod_mins, days = !!mod_days, mets$moderate),
                   IPAQ_MET_Light = ipaq_compute_met(minutes = !!light_mins, days = !!light_days, mets$light)
  )
  
  tmp <- dplyr::mutate(tmp, IPAQ_MET = ipaq_compute_sum(IPAQ_MET_Vigorous, IPAQ_MET_Moderate, IPAQ_MET_Light))
  tmp <- dplyr::mutate_all(tmp, zero2na)
  
  tmp <- dplyr::bind_cols(dplyr::select(data, -dplyr::one_of(names(data)[names(data) %in% names(tmp)])), 
                   tmp)
  
  # Create a some validator vectors, used for the MET factorial
  valid1 <- dplyr::select(data, !!light_mins, !!mod_mins, !!vig_mins)
  valid1 <- rowSums(valid1)
  
  valid2 <- dplyr::select(data, !!light_mins, !!mod_mins)
  valid2 <- rowSums(valid2)
  
  q5 <- unlist(dplyr::select(data, !!vig_mins))
  q3 <- unlist(dplyr::select(data, !!mod_mins))
  q1 <- unlist(dplyr::select(data, !!light_mins))
  q6 <- unlist(dplyr::select(data, !!vig_days))
  
  tmp <- dplyr::mutate(tmp, 
                IPAQ_Coded = ifelse( (IPAQ_MET_Vigorous >= 1500 & !!vig_days >= 3) | 
                                       (valid1 >= 7 & IPAQ_MET >= 3000) ,
                                     "High",
                                     ifelse((!!vig_mins >= 20 & !!vig_days >= 3) | 
                                              (valid2 >= 5 & (!!light_days >= 30 | !!mod_days >= 30) ), 
                                            "Moderate",
                                            "Low")))
  
  if(keep_all){
    tmp
  }else{
    dplyr::select(tmp,
           dplyr::contains("IPAQ_MET"),
           IPAQ_Coded)
  }
  
}

zero2na <- function(x) ifelse(x == 0, NA, x)

