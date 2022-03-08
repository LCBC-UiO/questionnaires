#' @rdname bdi
#' @title Calculate BDI scores
#' @description {
#' ```{r child="man/fragments/bdi/background.Rmd"}
#' ```
#' ```{r child="man/fragments/bdi/scoring.Rmd"}
#' ```
#' ## Data requirements
#' ```{r child="man/fragments/bdi/datareq.Rmd"}
#' ```
#' ## References
#' ```{r child="man/fragments/bdi/references.Rmd"}
#' ```
#' }
#' @return data.frame
#' @export
#' @examples
#' # Example of treatment of missing values
#' library(dplyr)
#' library(questionnaires)
#' data <- tibble(
#' bdi_01 = c(1, NA_real_, NA_real_, 2, 1),
#' bdi_02 = c(1, 1, NA_real_, 2, NA_real_)
#' )
#'
#' # Row with all components missing, gets sum 0
#' data %>%
#'   bind_cols(bdi_sum = bdi_compute_sum(data))
#' # Do not allow any missing values
#' data %>%
#'   bind_cols(bdi_sum = bdi_compute_sum(data, max_missing = 0))
#' # Allow one missing value
#' data %>%
#'   bind_cols(bdi_sum = bdi_compute_sum(data, max_missing = 2))
#' @importFrom dplyr transmute rename_all bind_cols
#' @template keep_all 
#' @template prefix
bdi_compute = function(data, 
                       cols = matches("bdi_[0-9][0-9]$"), 
                       max_missing = 0, 
                       prefix = "bdi_",
                       keep_all = TRUE){
  tmp <- transmute(data, 
                   sum = bdi_compute_sum(data, cols, max_missing = max_missing),
                   coded = bdi_factorise(sum))
  if(!is.null(prefix))
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
  
  if(keep_all)
    tmp <- bind_cols(data, tmp)
  
  tmp
}

#' @param data Data containing BDI data
#' @param cols Columns that contain BDI data
#' @param max_missing Maximum number of components allowed to be missing.
#'   Defaults to "0", and will return \code{NA} if missing any question. If set to
#'   \code{NULL} any missing component counts as 0, meaning if all BDI
#'   components are missing, the sum is still 0, not \code{NA}.
#' @importFrom dplyr select matches
#' @export
#' @describeIn bdi Compute the BDI sum based on a data.frame containing the BDI data. Returns a numeric vector.
bdi_compute_sum = function(data, cols = matches("bdi_[0-9][0-9]$"), max_missing = 0){
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

#' @param bdi_sum Sum of BDI questions, as summed by [bdi_compute_sum]
#' @export
#' @describeIn bdi Create a factor based on the BDI sum, with the cut-off points as described in original paper.
bdi_factorise <- function(bdi_sum){
  tmp <- case_when(
    bdi_sum <= 10 ~ "normal",
    bdi_sum <= 16 ~ "mild mood disturbance",
    bdi_sum <= 20 ~ "borderline clinical disturbance",
    bdi_sum <= 30 ~ "moderate depression",
    bdi_sum <= 40 ~ "severe depression",
    bdi_sum >  40 ~ "extreme depression"
  )
  factor(tmp, 
         levels = c("normal", 
                    "mild mood disturbance",
                    "borderline clinical disturbance",
                    "moderate depression",
                    "severe depression",
                    "extreme depression"),
         ordered = TRUE
  )
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
#' the format bdi_01_0 bdi_01_1 bdi_01_2 bdi_01_3,
#' where the first two numbers are the question 
#' number, and the last number is the option number.
#'
#' @inheritParams bdi_compute_sum
#' @param sep separator to use for the column names
#' @importFrom dplyr filter mutate group_by_at summarise ungroup '%>%'
#' @importFrom tidyr gather spread separate unite
#' @return data frame
#' @export
#' @examples 
#'   dat <- data.frame(
#'      ID = 1:4, 
#'      bdi_01_0 = c(NA,1, NA, NA),
#'      bdi_01_1 = c(1, NA, 1, NA),
#'      bdi_01_2 = c(NA, NA, 1, NA),
#'      bdi_01_3 = c(NA, NA, NA, NA),
#'      bdi_02_0 = c(1, NA, NA, NA),
#'      bdi_02_1 = c(NA,NA, NA, NA),
#'      bdi_02_2 = c(NA,1, NA, NA),
#'      bdi_02_3 = c(NA, NA, NA, 1)
#'   )
#'   bdi_restructure(dat)
bdi_restructure <- function(data, 
                            cols = matches("[0-9]_[0-9]"),
                            sep = "_"){
  gather(data, key, val, {{cols}}) %>% 
    filter(!is.na(val)) %>% 
    separate(key, c("key", "q", "val")) %>% 
    mutate(val = as.integer(val)) %>% 
    group_by_at(dplyr::vars(-val)) %>% 
    summarise(val = mean(val)) %>% 
    unite(key, c(key, q), sep = sep) %>% 
    spread(key, val) %>% 
    ungroup()
}

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("bdi_coded", "bdi",
                           "bdi_01", "val", "key"))

