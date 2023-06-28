#' Edinburgh handedness inventory 
#' 
#' Compute all variables of ehi, using 
#' other functions in this package. 
#' Will return the given data.frame with
#' three additional columns, the laterality
#' quotient (LQ), the laterality factor (Coded), and
#' the nominal laterality code (Nominal). 
#' 
#' ## Background
#' ```{r child="man/fragments/ehi/background.Rmd"} 
#' ```
#' ## Scoring
#' ```{r child="man/fragments/ehi/scoring.Rmd"} 
#' ```
#' ## Data requirements  
#' ```{r child="man/fragments/ehi/datareqs.Rmd"} 
#' ```
#' ## References
#' ```{r child="man/fragments/ehi/references.Rmd"} 
#' ```
#' @param data data.frame containing ehi data
#' @param cols tidyselected columns of all ehi data
#' @param writing numeric vector of writing preference (-2,-1,0,1,2)
#' @template keep_all
#' @template prefix
#' @param ... additional arguments to ehi_factorise_lqa
#' 
#' @return data.frame
#' @export
#' @family ehi_functions
#' @importFrom dplyr rename_all transmute bind_cols
ehi_compute = function(data, 
                       cols = matches("^ehi_[0-9][0-9]$"),
                       writing = ehi_01, 
                       ..., 
                       keep_all = TRUE,
                       prefix = "ehi_"){
  tmp <- transmute(data,
                   lq = ehi_compute_lq(data, cols = cols),
                   nominal = ehi_factorise_nominal( {{writing}} ),
                   lq_cat = ehi_factorise_lq(lq),
                   lqa_cat = ehi_factorise_lqa(lq, ...)
  )
  
  if(!is.null(prefix))
    tmp <- rename_all(tmp,
                      ~paste0(prefix, .x))
  
  if(keep_all)
    tmp <- bind_cols(data, tmp)
  
  tmp
}

#' Sum ehi columns 
#' 
#' Calculate the sum on non-NA
#' values in all columns in the specified direction(
#' 1 == sum all positives, -1 sum absolutes values of 
#' negatives)
#'
#' @param data  data.frame containing ehi data
#' @param cols  tidy-selection of all ehi columns
#' @param direction sum positive or negatives (1 for
#' positive, -1 for negative)
#' @importFrom dplyr transmute across
#' @return numeric vector
ehi_values <- function(data, 
                       cols = matches("^ehi_[0-9][0-9]$"),
                       direction = 1){
  tmp <- transmute(data, 
                   across({{cols}}, 
                          .fn = function(x) 
                            ehi_change(x, direction = direction)
                   ))
  rowSums(tmp, na.rm = TRUE)
}


#' Laterality Quotient
#' 
#' The laterality quotient is calculated
#' using all the answers on the ehi, with the
#' formula:
#' (pos-neg)/(pos+neg)*100 )
#' ## Background
#' ```{r child="man/fragments/ehi/background.Rmd"} 
#' ```
#' ## Scoring
#' ```{r child="man/fragments/ehi/scoring.Rmd"} 
#' ```
#' ## Data requirements  
#' ```{r child="man/fragments/ehi/datareqs.Rmd"} 
#' ```
#' ## References
#' ```{r child="man/fragments/ehi/references.Rmd"} 
#' ```
#' @param data data.frame containing ehi data
#' @param cols tidyselected columns of all ehi data
#'
#' @return numeric
#' @export
#' @family ehi_functions
ehi_compute_lq <- function(data, cols = matches("^ehi_[0-9][0-9]$")){
  pos <- ehi_values(data, cols, 1)
  neg <- ehi_values(data, cols, -1)
  ((pos-neg)/(pos+neg))*100 
}

# Factors ----
#' Nominal laterality factor
#' 
#' Using the answers to the first question 
#' on writing from the Edinburgh handedness inventory,
#' a nominal scale of three factors can be returned.
#'
#' @param writing numeric vector of writing preference (-2,-1,0,1,2)
#'
#' @return factor
#' @export
#' @family ehi_functions
#' @importFrom dplyr case_when
#' @examples
#' writing <- c(2, 2, -1, 0, 1, -2)
#' ehi_factorise_nominal(writing)
ehi_factorise_nominal <- function(writing = ehi_01){
  ehi_factorise_lqa(writing, 0, 0)
}

#' Factorise laterality quotient
#' 
#' While the laterality quotient is nice 
#' to use if your sample and variance is
#' large enough for analyses, in most
#' cases you will need to report the categories
#' of laterality your participants fall within.
#' This function takes the laterality quotient as
#' computed by [ehi_compute_lq] and creates a factor
#' using common specifications.
#' 
#' ## Background
#' ```{r child="man/fragments/ehi/background.Rmd"} 
#' ```
#' ## Scoring
#' ```{r child="man/fragments/ehi/scoring.Rmd"} 
#' ```
#' ## Data requirements  
#' ```{r child="man/fragments/ehi/datareqs.Rmd"} 
#' ```
#' ## References
#' ```{r child="man/fragments/ehi/references.Rmd"} 
#' ```
#' \itemize{
#'  \item{ehi_factorise_lq - }{returns original two-factor specification}
#'  \item{ehi_factorise_lqa - }{returns commonly used three-factor specification}
#' }
#'
#' @param lq numeric vector calculated by [ehi_compute_lq]
#' @param min minimum value for ambidexter specification (default = -70)
#' @param max maximum value for ambidexter specification (default = 70)
#' @param levels the levels for the lq component. Usually c("left", "ambidexter", "right").
#' @return factor
#' @export
#' @family ehi_functions
#' @examples
#' LQ <- c(1, 40, 70, -20, 0, 100, -90)
#' ehi_factorise_lq(LQ)
#' ehi_factorise_lqa(LQ)
#' ehi_factorise_lqa(LQ, min = -40, max = 60)
ehi_factorise_lq <- function(lq = ehi_lq){
  lq <- ifelse(lq == 0, 0.1, lq)
  ehi_factorise_lqa(lq, 0, 0, levels = c("left", "right"))
}

#' @rdname ehi_factorise_lq
#' @export
#' @importFrom dplyr case_when
ehi_factorise_lqa <- function(lq,
                              min = -70,
                              max = 70,
                              levels = c("left", "ambidexter", "right")){
  factor(
    case_when(
      is.na(lq) ~ NA_character_,
      lq > max  ~ "right",
      lq < min  ~ "left",
      TRUE ~ "ambidexter"),
    levels = levels
  )
}




#' Create vector with only correct values
#' 
#' Since the coding we have often uses 
#' negative numbers to indicate left-hand 
#' preferences, a specialized function is here
#' to return a vector with only the values asked
#' for.
#' 
#' If direction is set to 1, returns only positive
#' numbers, negative and 0 returns as NA.
#' If direction is set to -1, returns only negative
#' numbers, positive and 0 returns as NA.
#'
#' @param x numeric vector
#' @param direction either 1 for positive, -1 for negative
#'
#' @return numeric vector
ehi_change <- function(x, direction = 1){
  direction <- as.character(direction)
  direction <- match.arg(direction, c("1", "-1"))
  switch(direction,
         "1"  = abs(ifelse(x > 0, x, NA)),
         "-1" =  abs(ifelse(x < 0, x, NA))
  )
}
if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("ehi_01", 
                           "ehi_lq_cat",
                           "ehi_lqa_cat", 
                           "ehi_lq", 
                           "lq",
                           "ehi_nominal"))
