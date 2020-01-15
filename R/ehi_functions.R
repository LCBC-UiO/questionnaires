
#' Create vector with only correct values
#' 
#' Since the coding we have opten here uses 
#' negative numbers to indicate left-hand 
#' preferences, a specialized function is here
#' to return a vector with only the values asked
#' for.
#' 
#' If direction is set to 1, returns only positive
#' numbers, negative and 0 returns as NA.
#' If direction is set to -1, returns only negative
#' numbers, positive and 0 returns as NA.
#'
#' @param x numeric vector
#' @param direction either 1 for positive, -1 for negative
#'
#' @return numeric vector
ehi_change <- function(x, direction = 1){
  if(direction == 1){
    abs(ifelse(x > 0, x, NA))
  }else if(direction == -1){
    abs(ifelse(x < 0, x, NA))
  }else{
    stop(paste0("Direction '", direction, 
                "' is not valid. Choose '1' for positive or '-1' for negative"),
         call. = FALSE)
  }
}

#' Sum EHI columns 
#' 
#' Calculate the sum on non-NA
#' values in all columns in the specified direction(
#' 1 == sum all positives, -1 sum absolutes values of 
#' negatives)
#'
#' @param data  data.frame containing EHI data
#' @param cols  tidy-selection of all EHI columns
#' @param direction sum positive or negatives (1 for
#' positive, -1 for negative)
#'
#' @return numeric vector
ehi_values <- function(data, 
                       cols = matches("^EHI_[0-9][0-9]$"),
                       direction = 1){

  tmp <- dplyr::select(data, {{cols}} ) 
  tmp <- dplyr::mutate_all(tmp, ehi_change, direction = direction) 
  
  rowSums(tmp, na.rm = TRUE)
}


#' Laterality Quotient
#' 
#' The laterality quotient is calculated
#' using all the answes on the EHI, with the
#' formula:
#' (pos-neg)/(pos+neg)*100 )
#'
#' @param data data.frame containing EHI data
#' @param cols tidyselected columns of all EHI data
#'
#' @return numeric
#' @export
#' @family ehi_functions
ehi_compute_lq <- function(data, cols = matches("^EHI_[0-9][0-9]$")){
  
  pos <- ehi_values(data, cols, 1)
  neg <- ehi_values(data, cols, -1)
  
  ((pos-neg)/(pos+neg))*100 
}

# Factors ----
#' Nominal laterality factor
#' 
#' Using the answers to the first question 
#' on writing from the Edinburgh handedness inventory,
#' a nominal scale of three factors can be returned.
#'
#' @param writing numeric vector of writing prefereance [-2,-1,0,1,2]
#'
#' @return factor
#' @export
#' @family ehi_functions
#' @importFrom dplyr case_when
#' @examples
#' writing <- c(2, 2, -1, 0, 1, -2)
#' ehi_factorise_nominal(writing)
ehi_factorise_nominal <- function(writing = EHI_01){
  factor(
    dplyr::case_when(
      writing == 0 ~ "Ambidexterous", 
      writing > 0 ~ "Right", 
      writing < 0 ~ "Left"),
    levels = c("Left", "Ambidexterous", "Right")
  )
}

#' Factorise laterality quotient
#' 
#' While the laterality quotient is nice 
#' to use if your sample and variance is
#' large enough for analyses, in most
#' cases you will need to report the categories
#' of laterality your participants fall within.
#' This function takes the laterality quotient as
#' computed by [ehi_compute_lq] and creates a factor
#' using common specifications.
#' 
#' \itemize{
#'  \item{ehi_factorise_lq - }{returns original two-factor specification}
#'  \item{ehi_factorise_lqa - }{returns commonly used three-factor specification}
#' }
#'
#' @param lq numeric vector calculated by [ehi_compute_lq]
#' @param min minimum value for ambidexter specification (default = -70)
#' @param max maximum value for ambidexter specification (default = 70)
#'
#' @return factor
#' @export
#' @family ehi_functions
#' @examples
#' LQ <- c(1, 40, 70, -20, 0, 100, -90)
#' ehi_factorise_lq(LQ)
#' ehi_factorise_lqa(LQ)
#' ehi_factorise_lqa(LQ, min = -40, max = 60)
ehi_factorise_lq <- function(lq = EHI_LQ){
  factor(
    ifelse(lq >= 0,"Right","Left"),
    levels = c("Left", "Right")
  )
}

#' @rdname ehi_factorise_lq
#' @export
ehi_factorise_lqa <- function(lq = EHI_LQ,
                              min = -70,
                              max = 70){
  factor(
    dplyr::case_when(
      is.na(lq) ~ NA_character_,
      lq > max  ~ "Right",
      lq < min ~ "Left",
      TRUE ~ "Ambidexter"),
    levels = c("Left", "Ambidexter", "Right")
  )
}

#' Edinburgh handedness inventory 
#' 
#' Compute all variables of EHI, using 
#' other functions in this package. 
#' Will return the given data.frame with
#' three additional columns, the laterality
#' quotient (LQ), the laterality factor (Coded), and
#' the nominal laterality code (Nominal). 
#' 
#' @param data data.frame containing EHI data
#' @param cols tidyselected columns of all EHI data
#' @param writing numeric vector of writing prefereance [-2,-1,0,1,2]
#' @param keep_all logical, append to data.frame
#' @param ... additional arguments to ehi_factorise_lqa
#' 
#' @return data.frame
#' @export
#' @family ehi_functions
#' @importFrom dplyr mutate select
ehi_compute = function(data, 
                       cols = matches("^EHI_[0-9][0-9]$"),
                       writing = EHI_01, 
                       ..., 
                       keep_all = TRUE){

  tmp <- mutate(data,
                EHI_LQ = ehi_compute_lq(data),
                EHI_Nominal = ehi_factorise_nominal( {{writing}} ),
                EHI_LQ_cat = ehi_factorise_lq(EHI_LQ),
                EHI_LQA_cat = ehi_factorise_lq(EHI_LQ, ...)
  )
  
  if(keep_all){
    tmp
  }else{
    select(tmp, EHI_LQ, EHI_Nominal, EHI_LQ_cat, EHI_LQA_cat)
  }
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("EHI_01", "EHI_LQ_cat","EHI_LQA_cat", "EHI_LQ", "EHI_Nominal"))
