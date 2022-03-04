zero2na <- function(x) ifelse(x == 0, NA, x)

str_count <- function(x){
  unname(sapply(x, function(y) length(strsplit(y, "")[[1]])))
}
