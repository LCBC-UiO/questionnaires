#' Calculate sum of BDI
#'
#' @param data Data containing BDI data
#' @param cols Columns that contain BDI data
#'
#' @return numeric
#' @export
#' @family bdi_functions
#' @importFrom dplyr enquo select matches
bdi_compute_sum = function(data, cols = matches("BDI_[0-9][0-9]$"), min.answered = 18){
  cols = enquo(cols)
  
  tmp <- transmute(data, 
                N = rowSums(!is.na(select(data, !!cols))),
                BDI = rowSums(select(data, !!cols), na.rm=TRUE)
  )
  
  # If raw BDI is punched, calculate the sum
  ifelse(tmp$N < min.answered, NA, tmp$BDI)
        
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

#' Title
#'
#' @param data Data containing BDI data
#' @param cols Columns that contain BDI data
#' @param keep_all logical, append to data.frame
#'
#' @return data.frame
#' @export
#' @family bdi_functions
bdi_compute = function(data, cols = matches("BDI_[0-9][0-9]$"), keep_all = TRUE){
  
  tmp <- mutate(data, BDI = bdi_compute_sum(data, cols))
  tmp <- mutate(tmp, BDI_Coded = bdi_factorise(BDI))
  
  
  if(keep_all){
    tmp
  }else{
    select(tmp, BDI, BDI_Coded)
  }
}
  
if(getRversion() >= "2.15.1")  utils::globalVariables(c("BDI_Coded", "BDI"))

