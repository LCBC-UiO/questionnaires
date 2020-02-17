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
#' @family gds_functions
#' @examples
#' gds_values()
#' gds_values(yes = "YES", no = "NO")
gds_values = function(yes = 1, no = 0){
  list(yes = yes, no = no)
}


#' Binerise GDS values
#' 
#' internal function to make all "yes" answers
#' equal to 1, and all "no" to 0. This for
#' convenience of calculations later.
#'
#' @param x vector of yes and no coding
#' @inheritParams gds_compute_sum
#'
#' @return vector of 0's and 1's
#' @export
#' @family gds_functions
#' @examples
#' gds_binary(c(1,1,0,NA,1), gds_values(1,0))
#' gds_binary(c("y","y","n",NA,"y"), gds_values(yes = "y", no = "n"))
gds_binary <- function(x, values = gds_values()){
  
  stopifnot(is_gds_values(values))
  
  dplyr::case_when(
    x == values$no ~ 1,
    x == values$yes ~ 0,
    TRUE ~ NA_real_
  )
} 

#' Change coding of GDS to correct numeric values
#' 
#' Necessary step for computing the total score
#' 
#' @param reverse reverse logic
#' @inheritParams gds_compute_sum
#' 
#' @family gds_functions
gds_alter_values <- function(data, values = gds_values(), reverse = FALSE,
                               cols = dplyr::matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$")){

  if(reverse){
    values <- rev(unname(values))
    names(values) <- c("yes", "no")
  }
  
  dplyr::mutate_at(data, 
                   dplyr::vars( {{cols}} ), 
                   gds_binary, values = values)
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
gds_compute_sum <- function(data, 
                            cols = dplyr::matches("GDS_[0-3][0-9]$"),
                            cols_rev = dplyr::matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                            values = gds_values()
){
  
  stopifnot(is_gds_values(values))
  
  tmp <- dplyr::select(data, {{cols}} )
  
  tmp <- gds_alter_values(tmp, values, reverse = FALSE, cols_rev)
  tmp <- gds_alter_values(tmp, values, reverse = TRUE, -cols_rev)
  
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
                        cols = dplyr::matches("GDS_[0-9][0-9]$"),
                        cols_rev = dplyr::matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                        values = gds_values(),
                        keep_all = TRUE){
  
  stopifnot(is_gds_values(values))
  
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

is_gds_values <- function(x){
  j <- length(x) == 2
  k <- all(names(gds_values("Yes", "No")) == c("yes", "no"))
  
  all(c(k,j))
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("GDS"))
