#' Calculate sum of BDI
#'
#' @param data Data containing BDI data
#' @param cols Columns that contain BDI data
#' @param max_missing Maximum number of components allowed to be missing.
#'   Defaults to "0", and will return \code{NA} if missing any question. If set to
#'   \code{NULL} any missing component counts as 0, meaning if all BDI
#'   components are missing, the sum is still 0, not \code{NA}.
#'
#' @return numeric
#' @export
#' @family bdi_functions
#' @importFrom dplyr enquo select matches
#' @examples
#' # Example of treatment of missing values
#' library(dplyr)
#' library(Questionnaires)
#' data <- tibble(
#' BDI_01 = c(1, NA_real_, NA_real_, 2, 1),
#' BDI_02 = c(1, 1, NA_real_, 2, NA_real_)
#' )
#'
#' # Row with all components missing, gets sum 0
#' data %>%
#'   bind_cols(BDI_sum = bdi_compute_sum(data))
#' # Do not allow any missing values
#' data %>%
#'   bind_cols(BDI_sum = bdi_compute_sum(data, max_missing = 0))
#' # Allow one missing value
#' data %>%
#'   bind_cols(BDI_sum = bdi_compute_sum(data, max_missing = 2))
#' 
bdi_compute_sum = function(data, cols = matches("BDI_[0-9][0-9]$"), max_missing = 0){

  
  # If raw BDI is punched, calculate the sum
  if(is.null(max_missing)){
    rowSums(select(data, {{cols}} ), na.rm = TRUE)  
  } else {
    stopifnot(max_missing >= 0)
    apply(select(data, {{cols}} ), 1, function(x){
      if(sum(is.na(x)) > max_missing){
        NA_real_
      } else {
        sum(x, na.rm = TRUE)
      }
    })
  }
}


#' Factorize BDI sum
#'
#' @param bdi_sum Sum of BDI questions, as summed by [bdi_compute_sum]
#'
#' @return factor
#' @export
#' @family bdi_functions
bdi_factorise <- function(bdi_sum = BDI){
  
  # If there is a sum, categorise it according to original paper.
  tmp <- ifelse(bdi_sum<=13, 
                "Minimal depression",
                ifelse(bdi_sum>=14 & bdi_sum <=19, 
                       "Mild depression",
                       ifelse(bdi_sum>=20 & bdi_sum <= 28, 
                              "Moderate depression",
                              "Severe depression"))
  )
  
  factor(tmp, 
         levels = c("Minimal depression", 
                    "Mild depression",
                    "Moderate depression",
                    "Severe depression")
  )
}

#' Compute all BDI data from questionnaire
#'
#' @inheritParams bdi_compute_sum
#' @param keep_all logical, append to data.frame
#'
#' @return data.frame
#' @export
#' @family bdi_functions
bdi_compute = function(data, 
                       cols = matches("BDI_[0-9][0-9]$"), 
                       max_missing = 0, 
                       keep_all = TRUE){
  
  # If BDI does no exists in the data, make NAs for missing
  tmp <- if("BDI" %in% names(data)){
    data
  }else{
    mutate(data, BDI = NA)
  }
  
  tmp <- mutate(tmp, 
                BDI = ifelse(!is.na(BDI_01), 
                             bdi_compute_sum(data, cols, max_missing = max_missing), 
                             BDI))
  
  tmp <- mutate(tmp, BDI_Coded = bdi_factorise(BDI))
  
  if(keep_all){
    tmp
  }else{
    select(tmp, BDI, BDI_Coded)
  }
}
  
if(getRversion() >= "2.15.1")  utils::globalVariables(c("BDI_Coded", "BDI"))

