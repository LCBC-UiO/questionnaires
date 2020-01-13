
gds_binary <- function(x, value) ifelse(x != value, 1, 0)

#' @importFrom dplyr mutate_at vars
gds_compute_values <- function(data, value = 1,
                               cols = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$")){
  mutate_at(data, vars( {{cols}} ), gds_binary, value = value)
}


#' Specify coding scheme for GDS questions
#' 
#' Function to easily set the response coding used
#' in the GDS data.
#'
#' @param yes value indicating a positive answer
#' @param no value indicating a negative answer
#'
#' @return list of yes and no values
#' @export
#'
#' @examples
#' gds_values()
#' gds_values(yes = "YES", no = "NO")
gds_values = function(yes = 1, no = 0){
  list(yes = yes, no = no)
}

#' Compute the GDS sum
#' 
#' Calculates the GDS total score based
#' on the columns indicated, with reversal
#' of the columns indicated.
#' 
#' @param data data.frame with GDS data in it
#'
#' @param cols GDS data columns
#' @param cols_rev Columns for reversal of binary code
#' @param values named vector of 2 providing the coding for Yes and No answers c(Yes = 1, No = 2)
#' @export
#' @return numeric
#' @family gds_functions
#' @importFrom dplyr select
gds_compute_sum <- function(data, 
                            cols = matches("GDS_[0-3][0-9]$"),
                            cols_rev = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                            values = gds_values()
){
  
  tmp <- select(data, {{cols}} )
  
  tmp <- gds_compute_values(tmp, values$yes, cols_rev)
  tmp <- gds_compute_values(tmp, values$no, -cols_rev)
  
  tt <- rowSums(tmp)

  tt
}

#' Factorise the GDS sum
#'
#' @param gds_sum numeric vector of GDS sums
#'
#' @return factor
#' @export
#' @family gds_functions
gds_factorise <- function(gds_sum){
  tmp <- case_when(
    gds_sum <= 9 ~ "Normal",
    gds_sum >= 10  & gds_sum <= 19 ~ "Mild depression",
    gds_sum > 19 ~ "Severe depression"
  )
  
  factor(tmp,
         levels = c("Normal", "Mild depression", "Severe depression"),
         ordered = TRUE)
}

#' Compute GDS score
#' 
#' @param data data.frame with GDS data in it
#'
#' @param cols GDS data columns
#' @param cols_rev Columns for reversal of binary code
#' @param values named vector of 2 providing the coding for Yes and No answers c(Yes = 1, No = 2)
#' @param keep_all logical, append to data.frame
#' @family gds_functions
#' @return data frame
#' @export
#' @importFrom dplyr mutate bind_cols select one_of
gds_compute <- function(data, 
                        cols = matches("GDS_[0-9][0-9]$"),
                        cols_rev = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                        values = gds_values(),
                        keep_all = TRUE){
  
  
  tmp <- data.frame(GDS = gds_compute_sum(data = data, 
                                          cols = cols,
                                          cols_rev = cols_rev,
                                          values = values)
  )
  
  tmp <- mutate(tmp, GDS_Coded = gds_factorise(GDS))
  
  if(keep_all){
    bind_cols(select(data,
                     -one_of(names(data)[names(data) %in% names(tmp)])), 
              tmp)
  }else{
    tmp
  }
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("GDS"))
