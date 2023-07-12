

#' Compute weighted zygocity
#'
#' ```{r child="man/fragments/zygocity/background.Rmd"}
#' ```
#' 
#' ```{r child="man/fragments/zygocity/scoring.Rmd"}
#' ```
#' 
##' ```{r child="man/fragments/zygocity/score_coding.Rmd"}
#' ```
#' 
#' ```{r child="man/fragments/zygocity/equation.Rmd"}
#' ```
#' 
#' ```{r child="man/fragments/zygocity/datareq.Rmd"}
#' ```
#' 
#' ```{r child="man/fragments/zygocity/references.Rmd"}
#' ```
#'  
#' @template data 
#' @param twin_col column that codes for twin pairs. Each twin
#'    should have the same identifier here.
#' @param cols columns that contain the zygocity data. Use tidy-selectors
#' @template prefix 
#' @param recode logical indicating if data should be recoded
#'       from 1-5(7) to -1. 0. 1.
#' @template keep_all 
#' @importFrom dplyr mutate transmute ungroup rowwise bind_cols
#' @importFrom dplyr rename_all select group_by row_number
#' @return data.frame with computed values
#' @export
zygo_compute <- function(data,
                         twin_col,
                         cols,
                         recode = TRUE,
                         prefix = "zygo_",
                         keep_all = FALSE){
  .zc <- function(...){
    suppressWarnings(zygo_calc(..., recode = recode))
  }
  if(missing(twin_col))
    cli::cli_abort(sprintf("Missing column identifying twins: 'twin_col'"))
  if(missing(cols))
    cli::cli_abort(sprintf("Missing column identifying Zygocity data. Use tidy-selectors to indicate correct data."))
  tmp <- ungroup(data)
  tmp <- mutate(tmp, .id = row_number())
  tmp <- zygo_type(tmp, {{twin_col}}, {{cols}})
  tmp <- group_by(tmp, .id)
  tmpr <- transmute(
    tmp,
    .id,
    .type,
    eye        = .zc(eye, "eye", .type),
    drop       = .zc(drop, "drop", .type),
    stranger   = .zc(stranger, "stranger", .type),
    dexterity  = .zc(dexterity, "dexterity", .type),
    voice      = .zc(voice, "voice", .type),
    belief     = .zc(belief, "belief", .type)
  )
  tmpr <- zygo_compute_sum(tmpr, .type)
  tmpr <- select(tmpr, -.id, -.type)
  
  if(!is.null(prefix))
    tmpr <- rename_all(tmpr, ~paste0(prefix, .x))
  
  if(keep_all) 
    tmpr <- bind_cols(data, tmpr)
  
  tmpr
}

#' Find how many twins have answered
#' 
#' The zygocity calculations are different depending
#' on wheather both twins have answered the
#' questionnaire or not. This convenience function
#' help determine, based on the column coding for 
#' twin pairs, if one or two twins are present in 
#' the data with complete viable data.
#' If both twins are in the data, but one twin has
#' incomplete data, the function will return 
#' "single" for the remaining twin.
#'
#' @inheritParams zygo_compute 
#'
#' @return full data frame with twin type appended
#' @export
#' @importFrom dplyr starts_with group_by mutate
#' @importFrom dplyr case_when ungroup select
#' @examples
zygo_type <- function(data, 
                      twin_col, 
                      cols = starts_with("zygo")) {
  tmp <- group_by(data, {{twin_col}})
  tmp <- mutate(tmp,
                .tt = sum(c_across({{cols}})),
                .type = case_when(
                  is.na(.tt) ~ "single",
                  length({{twin_col}}) == 1 ~ "single",
                  length({{twin_col}}) == 2 ~ "pair"
                )
  )
  tmp <- ungroup(tmp, {{twin_col}})
  select(tmp, -.tt)  
}

zygo_compute_sum <- function(data, type_col){
  tmpr <- rowwise(data)
  tmpr <- mutate(
    tmpr,
    score = case_when(
      {{type_col}} == "single" ~ 
        sum(c_across(c(voice, drop, stranger, dexterity, belief))) -0.087,
      {{type_col}} == "pair" ~ 
        sum(c_across(c(eye, drop, stranger, dexterity, belief))) +0.007
    ),
    zygocity = case_when(
      sign(score) == 1 ~ "monozygote",
      sign(score) == -1 ~ "dizygote"
    )
  )
  ungroup(tmpr)
}

#' Zygocity - Calculate item
#' 
#' ```{r child="man/fragments/zygocity/scoring.Rmd"}
#' ```
#'
#' @param x integer vector of answers to one
#'          of the questionnaire questions.
#'          Should not be longer than 2.
#' @param type type of question the vector is
#'          from. "drop", "stranger, "dexterity",
#'          "voice", "eye", or "belief".
#' @param n string indicating number of twins in 
#'          the pair available. Either "single" or "pair".
#' @param recode logical indicating if data should be 
#'          recoded from 1-5(7) to -1. 0. 1.
#' @return single value of calculated score based
#'         on recoded vector and multiplied with
#'         correct factor weight.
#' @export
#'
#' @examples
#' zygo_calc(c(1), type = "eye")
#' zygo_calc(c(1,3), type = "belief", n = "pair")
#' zygo_calc(c(4), type = "voice")
zygo_calc <- function(x, type, n = "single", recode = TRUE){
  if(recode)
    x <- zygo_recode(x, type)
  zygo_weighted(x, type, n)
}

#' Calculate weighted zygocity item
#' 
#' Calculate the item score of a question.
#' Function takes a single vector, with information
#' on the question type and the twin type ('single' 
#' or 'pair') and calculates the zygocity item score.
#'
#' @param x vector or recoded zygocity data (-1, 0, 1)
#' @param type string. one of 'drop', 'stranger', 
#'    'dexterity', 'voice', 'eye' or 'belief.'
#' @param n string, 'pair' if both twins have answered,
#'  'single' if not.
#'
#' @return numeric vector of weighted data
#' @export
#'
#' @examples
zygo_weighted <- function(x, type, n = "single"){
  n <- match.arg(n, c("single", "pair"))
  type <- match.arg(type,
                    c("drop", 
                      "stranger",
                      "dexterity",
                      "voice",
                      "eye",
                      "belief"))
  if(length(na.omit(x)) == 0)
    return(NA)
  
  if(n == "single"){
    if(length(x) > 1)
      cli::cli_abort("Vector is longer than 1, this is not valid for single twin answers")
    
    fct <- switch(type,
                  "drop"       = 1.494,
                  "stranger"   = 0.647,
                  "dexterity"  = 0.458,
                  "voice"      = 0.347,
                  "belief"     = 0.417
    )
  }else{
    fct <- switch(type,
                  "drop"       = 2.111,
                  "stranger"   = 0.691,
                  "dexterity"  = 0.366,
                  "eye"        = 0.394,
                  "belief"     = 0.481
    )
    x <- mean(x)
  }
  
  if(is.null(fct)){
    cli::cli_warn(sprintf(
      "Cannot calculate '%s' for '%s' data",
      type, n))
    return(NA)
  }
  
  return(x*fct)
}

#' Zygocity - recode variables
#'
#' ```{r child="man/fragments/zygocity/score_coding.Rmd"}
#' ```
#' 
#' @param x vector of numbers, either 1:3 or 1:4
#' @param type Type of question to recode. Can either
#' be 05, 06, 07 or 08, or drop, stranger, dexterity, 
#' voice, eye or belief.
#'
#' @return return a vector with 0, -1 or 1.
#' @export
#'
#' @examples
#' zygo_recode(c(1:4, NA), type = "eye")
#' zygo_recode(c(1:4, NA), type = "voice")
#' zygo_recode(c(1:3, NA), type = "drop")
zygo_recode <- function(x, type){
  if(!type %in% c("05", "06",
                  "07", "08"))
    type <- switch (type,
                    "drop"       = "05",
                    "stranger"   = "06",
                    "dexterity"  = "07",
                    "voice"      = "07",
                    "eye"        = "07",
                    "belief"     = "08"
    )
  type <- match.arg(type,
                    c("05", "06",
                      "07", "08"))
  func <- eval(parse(text=sprintf("zygo_recode_%s", type)))
  func(x)
}

zygo_recode_05 <- function(x){
  stopifnot(all(unique(x) %in% c(1:3, NA)))
  x[x == 2] <- -1
  x[x == 3] <- 0
  x
}

zygo_recode_08 <- zygo_recode_05

zygo_recode_06 <- function(x){
  stopifnot(all(unique(x) %in% c(1:4, NA)))
  x[x == 3] <- -1
  x[x %in% c(2,4)] <- 0
  x
}

zygo_recode_07 <- zygo_recode_06

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c(".id",
                           ".tt",
                           ".type",
                           "stranger",
                           "dexterity",
                           "voice",
                           "belief",
                           "eye",
                           "score"))
