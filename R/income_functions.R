#' Turn income bins to mean of bin
#'
#' Older collected income data for LCBC
#' collected income information in 7 bins.
#' Newer data collects continuous income data.
#' This function converts binned income data 
#' from these 7 categories into the mean
#' income value for each bin.
#'
#' @param x income bin vector
#'
#' @return numeric
#' @export
#'
#' @examples
#' x <- c("< 200k", "600k - 699k", "400k - 499k", 
#'        "> 700k", "500k - 599k", "300k - 399k", 
#'         "200k - 299k")
#' income_bin2nok(x)
income_bin2nok = function(x){
  tmp <- gsub("[90]|k|<|>| |-","", as.character(x)) 
  
  tmp <- sapply(tmp, function(x) strsplit(x,"")[[1]][1])
  tmp <- as.integer(tmp)*100000 
  
  dplyr::case_when(
    tmp == 200000 ~ 100000,
    tmp == 700000 ~ 800000,
    TRUE ~ tmp + 50000
  )
}



#' Translate NOK to other currency
#' 
#' In order to compare income in Norway
#' to other countries, currency concersions
#' might be necessary. This function multiplies
#' with the rate provided. 
#'
#' @param x currency
#' @param rate currency translation rate (defaul 0.10 for euro)
#'
#' @return numeric
#' @export
#'
#' @examples
#' income_nok2other(c(100, 2930, 13649))
#' income_nok2other(c(100, 2930, 13649), 0.5)
income_nok2other <- function(x, rate = .10){
  x * rate
}
