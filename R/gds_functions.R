
gds_binary <- function(x, value) ifelse(x != value, 1, 0)

#' @importFrom dplyr enquo mutate_at vars
gds_compute_values <- function(data, value = 1,
                               cols = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$")){
  cols <- enquo(cols)
  
  mutate_at(data, vars(!!cols), gds_binary, value = value)
}


#' Compute the GDS sum
#' 
#' @param data data.frame with GDS data in it
#'
#' @param cols GDS data columns
#' @param cols_alter Columns for reversal of binary code
#' @param values named vector of 2 providing the coding for Yes and No answers c(Yes = 1, No = 2)
#' @export
#' @return numeric
#' @family gds_functions
#' @importFrom dplyr enquo select
gds_compute_sum <- function(data, 
                            cols = matches("GDS_[0-3][0-9]$"),
                            cols_alter = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                            values = c(Yes = 1, No = 0)
){
  cols <- enquo(cols)
  
  tmp <- select(data, !!cols)
  
  tmp <- gds_compute_values(tmp, values["Yes"], cols_alter)
  tmp <- gds_compute_values(tmp, values["No"], -cols_alter)
  
  rowSums(tmp)
  
}

#' Factorise the GDS sum
#'
#' @param gds_sum numeric vector of GDS sums
#'
#' @return factor
#' @export
#' @family gds_functions
gds_factorise <- function(gds_sum){
  tmp <- ifelse((gds_sum <= 9), 
         "Normal", 
         ifelse((10 <= gds_sum & gds_sum <= 19), 
                "Mild depression",
                "Severe deppression"))
  
  factor(tmp,
         levels = c("Normal", "Mild depression", "Severe depression"))
}

#' Title
#' 
#' @param data data.frame with GDS data in it
#'
#' @param cols GDS data columns
#' @param cols_alter Columns for reversal of binary code
#' @param values named vector of 2 providing the coding for Yes and No answers c(Yes = 1, No = 2)
#' @param keep_all logical, append to data.frame
#' @family gds_functions
#' @return data frame
#' @export
#' @importFrom dplyr mutate bind_cols select one_of
gds_compute <- function(data, 
                        cols = matches("GDS_[0-9][0-9]$"),
                        cols_alter = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                        values = c(Yes = 1, No = 0),
                        keep_all = TRUE){
  
  
  tmp <- data.frame(GDS = gds_compute_sum(data = data, 
                                          cols = cols,
                                          cols_alter = cols_alter,
                                          values = values)
  )
  
  tmp <- mutate(tmp, GDS_Coded = gds_factorise(GDS))
  
  
  if(keep_all){
    bind_cols(select(data, -one_of(names(data)[names(data) %in% names(tmp)])), 
              tmp)
  }else{
    tmp
  }
}
