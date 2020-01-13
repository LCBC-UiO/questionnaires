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
#' @importFrom dplyr case_when
#' @return factor
#' @export
#' @family bdi_functions
bdi_factorise <- function(bdi_sum = BDI){
  
  # If there is a sum, categorise it according to original paper.
  tmp <- dplyr::case_when(
    bdi_sum <= 13 ~ "Minimal depression",
    bdi_sum>=14 & bdi_sum <=19 ~ "Mild depression",
    bdi_sum>=20 & bdi_sum <= 28 ~ "Moderate depression",
    bdi_sum > 28 ~ "Severe depression"
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
#' Computes the sum and factorises the sum into 
#' the four BDI categories based on the sum
#'
#' @inheritParams bdi_compute_sum
#' @param predicate logical statement on which data 
#' to include in the summing function
#' @param keep_all logical, append to data.frame
#' #TODO: create option to make your own name
#' @return data.frame
#' @export
#' @family bdi_functions
bdi_compute = function(data, 
                       cols = matches("BDI_[0-9][0-9]$"), 
                       max_missing = 0, 
                       predicate = rep(TRUE, nrow(data)),
                       keep_all = TRUE){
  
  # If BDI does not exists in the data, make NAs for missing
  tmp <- if("BDI" %in% names(data)){
    data
  }else{
    mutate(data, BDI = NA)
  }
  
  tmp <- mutate(tmp, 
                BDI = ifelse({{predicate}}, 
                             bdi_compute_sum(data, cols, max_missing = max_missing), 
                             BDI))
  
  tmp <- mutate(tmp, BDI_Coded = bdi_factorise(BDI))
  
  if(keep_all){
    tmp
  }else{
    select(tmp, BDI, BDI_Coded)
  }
}

#' Restructure BDI questions from wide format
#' 
#' If data come from Nettskjema, the structure is
#' in wide format, with each question option as
#' columns, creating 21*4 columns of data. 
#' This function allows you to gather and create
#' single columns for questions. 
#' 
#' The columns must adhere to some specific logic to work.
#' It is recommended that the column names are in 
#' the format BDI_01_0 BDI_01_1 BDI_01_2 BDI_01_3,
#' where the first two numbers are the question 
#' number, and the last number is the option number.
#'
#' @inheritParams bdi_compute_sum
#' @param sep separator to use for the colum names
#' @importFrom dplyr filter mutate group_by_at summarise ungroup '%>%'
#' @importFrom tidyr gather spread separate unite
#' @return data frame
#' @export
#' @examples 
#'   dat <- data.frame(
#'      ID = 1:4, 
#'      BDI_01_0 = c(NA,1, NA, NA),
#'      BDI_01_1 = c(1, NA, 1, NA),
#'      BDI_01_2 = c(NA, NA, 1, NA),
#'      BDI_01_3 = c(NA, NA, NA, NA),
#'      BDI_02_0 = c(1, NA, NA, NA),
#'      BDI_02_1 = c(NA,NA, NA, NA),
#'      BDI_02_2 = c(NA,1, NA, NA),
#'      BDI_02_3 = c(NA, NA, NA, 1)
#'   )
#'   bdi_restructure(dat)
bdi_restructure <- function(data, 
                            cols = matches("[0-9]_[0-9]"),
                            sep = "_"){
  data %>% 
    tidyr::gather(key, val, {{cols}}) %>% 
    dplyr::filter(!is.na(val)) %>% 
    tidyr::separate(key, c("key", "q", "val")) %>% 
    dplyr::mutate(val = as.integer(val)) %>% 
    dplyr::group_by_at(dplyr::vars(-val)) %>% 
    dplyr::summarise_at(vars(val), mean) %>% 
    tidyr::unite(key, c(key, q), sep = sep) %>% 
    tidyr::spread(key, val) %>% 
    dplyr::ungroup()
}
  
if(getRversion() >= "2.15.1")  utils::globalVariables(c("BDI_Coded", "BDI",
                                                        "BDI_01", "val", "key"))

