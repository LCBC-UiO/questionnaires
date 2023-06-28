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


#' Binarise GDS values
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
#' @importFrom dplyr case_when
#' @examples
#' gds_binary(c(1,1,0,NA,1), gds_values(1,0))
#' gds_binary(c("y","y","n",NA,"y"), gds_values(yes = "y", no = "n"))
gds_binary <- function(x, values = gds_values()){
  stopifnot(is_gds_values(values))
  case_when(
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
#' @importFrom dplyr matches mutate across
#' @family gds_functions
gds_alter_values <- function(data, 
                             values = gds_values(), 
                             reverse = FALSE,
                             cols = matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$")){
  
  if(reverse){
    values <- rev(unname(values))
    names(values) <- c("yes", "no")
  }
  
  mutate(data, 
         across({{cols}}, 
                ~ gds_binary(.x, values = values))
  )
}


#' Compute the GDS sum
#' 
#' @param data data.frame with GDS data in it
#' @param cols GDS data columns
#' @param cols_rev Columns for reversal of binary code
#' @param values named vector of 2 providing the coding for Yes and No answers c(Yes = 1, No = 2)
#' @export
#' @return numeric
#' @importFrom dplyr select
#' @family gds_functions
#' @describeIn gds_compute Calculate the total GDS score
gds_compute_sum <- function(data, 
                            cols = dplyr::matches("[0-3][0-9]$"),
                            cols_rev = dplyr::matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                            values = gds_values()
){
  stopifnot(is_gds_values(values))
  tmp <- select(data, {{cols}} )
  tmp <- gds_alter_values(tmp, 
                          values, 
                          reverse = FALSE, 
                          cols_rev)
  tmp <- gds_alter_values(tmp, 
                          values, 
                          reverse = TRUE, 
                          -cols_rev)
  rowSums(tmp)
}

#' Factorise the GDS sum
#'
#' @param gds_sum numeric vector of GDS sums
#' @return factor
#' @export
#' @family gds_functions
#' @describeIn gds_compute Create a factor from the sum of the GDS scores
gds_factorise <- function(gds_sum){
  tmp <- case_when(
    gds_sum <= 9 ~ "normal",
    gds_sum >= 10  & gds_sum <= 19 ~ "mild depression",
    gds_sum > 19 ~ "severe depression"
  )
  factor(tmp,
         levels = c("normal", "mild depression", "severe depression"),
         ordered = TRUE)
}

#' Compute GDS score
#' 
#' ```{r child="man/fragments/gds/background.Rmd"}
#' ```
#' ##Scoring
#' ```{r child="man/fragments/gds/scoring.Rmd"}
#' ```
#' ## Data requirements
#' ```{r child="man/fragments/gds/datareq.Rmd"}
#' ```
#' ## References
#' ```{r child="man/fragments/gds/references.Rmd"}
#' ```
#' 
#' @param data data.frame with GDS data in it
#' @param cols GDS data columns
#' @param cols_rev Columns for reversal of binary code
#' @param values named vector of 2 providing the coding for Yes and No answers c(Yes = 1, No = 2)
#' @template keep_all 
#' @template prefix
#' @family gds_functions
#' @return data frame
#' @export
#' @importFrom dplyr mutate bind_cols select one_of matches
gds_compute <- function(data, 
                        cols = dplyr::matches("[0-9][0-9]$"),
                        cols_rev = dplyr::matches("01$|05$|07$|09$|15$|19$|21$|27$|29$|30$"),
                        values = gds_values(),
                        prefix = "gds_",
                        keep_all = TRUE){
  
  stopifnot(is_gds_values(values))
  
  tmp <- data.frame(
    sum = gds_compute_sum(data = data, 
                          cols = cols,
                          cols_rev = cols_rev,
                          values = values)
  )
  tmp <- mutate(tmp, coded = gds_factorise(sum))
  if(!is.null(prefix)){
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
  }
  
  if(keep_all){
    tmp <- bind_cols(data, tmp)
  }
  tmp
}

is_gds_values <- function(x){
  j <- length(x) == 2
  k <- all(names(gds_values("Yes", "No")) == c("yes", "no"))
  all(c(k,j))
}

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("sum"))
