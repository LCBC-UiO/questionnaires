
ehi_change <- function(x, direction = 1){
  if(direction == 1){
    abs(ifelse(x > 0, x, 1))
  }else if(direction == -1){
    abs(ifelse(x < 0, x, 1))
  }else{
    stop(paste0("Direction '", direction, "' is not valid. Choose '1' for positive or '-1' for negative"))
  }
}

ehi_values <- function(data, 
                       cols = matches("^EHI_[0-9][0-9]$"),
                       direction = 1){
  cols = dplyr::enquo(cols)
  
  tmp <- dplyr::select(data, !!cols) 
  tmp <- dplyr::mutate_all(tmp, ehi_change, direction = direction) 
  
  rowSums(tmp)
  
}


#' Edinburgh handedness inventory LE
#'
#' @param data data.frame containing EHI data
#' @param cols tidyselected columns of all EHI data
#'
#' @return numeric
#' @export
#' @family ehi_functions
ehi_compute_le <- function(data, cols = matches("^EHI_[0-9][0-9]$")){
  
  pos <- ehi_values(data, cols, 1)
  neg <- ehi_values(data, cols, -1)
  
  ((pos-neg)/(pos+neg))*100 
}

#' Edinburgh handedness inventory nominal
#'
#' @param writing numeric vector of writing prefereance [-2,-1,0,1,2]
#'
#' @return factor
#' @export
#' @family ehi_functions
ehi_factorise_nominal <- function(writing = EHI_01){
  factor(
    ifelse(writing > 0, "Right", 
           ifelse(writing == 0, "Ambidexterous", 
                  "Left")),
    levels = c("Left", "Ambidexterious", "Right")
  )
}

#' Edinburgh handedness inventory code
#'
#' @param le numeric vector calculated by [ehi_compute_le]
#'
#' @return factor
#' @export
#' @family ehi_functions
ehi_factorise_le <- function(le = EHI_LE){
  factor(
    ifelse(le >= 0,"Right","Left"),
    levels = c("Left", "Right")
  )
}

#' Edinburgh handedness inventory 
#' 
#' Compute all variables of EHI 
#' 
#' @param data data.frame containing EHI data
#' @param cols tidyselected columns of all EHI data
#' @param writing numeric vector of writing prefereance [-2,-1,0,1,2]
#' @param keep_all logical, append to data.frame
#' 
#' @return data.frame
#' @export
#' @family ehi_functions
#' @importFrom dplyr mutate select
ehi_compute = function(data, cols = matches("^EHI_[0-9][0-9]$"), writing = EHI_01, keep_all = TRUE){
  writing = enquo(writing)
  
  le <- ehi_compute_le(data, cols)
  sum <- ehi_factorise_le(le)
  
  tmp <- mutate(data,
                EHI_LE = le,
                EHI_Nominal = ehi_factorise_nominal(!!writing),
                EHI_Coded = sum
  )
  
  if(keep_all){
    tmp
  }else{
    select(tmp, EHI_LE, EHI_Nominal, EHI_Coded)
  }
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("EHI_01", "EHI_Coded", "EHI_LE", "EHI_Nominal"))
