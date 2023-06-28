

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
#' @template prefix 
#' @template keep_all 
#'
#' @return data.frame with computed values
#' @export
#' @importFrom dplyr mutate transmute ungroup rowwise rows_patch group_by
#' @importFrom dplyr rename_all select bind_cols group_by row_number
#'
# #' @examples
zygo_compute <- function(data,
                         twin_col,
                         prefix = "zygo_",
                         keep_all = FALSE){
  tmp <- ungroup(data)
  tmp <- mutate(tmp, .id = row_number())
  tmpr <- dplyr::rowwise(tmp)
  tmpr <- transmute(tmpr,
            .id,
            drop       = zygo_calc(drop, "drop", "single"),
            stranger   = zygo_calc(stranger, "stranger", "single"),
            dexterity  = zygo_calc(dexterity, "dexterity", "single"),
            voice      = zygo_calc(voice, "voice", "single"),
            belief     = zygo_calc(belief, "belief", "single"),
            eye        = NA_real_
            )
  
  tmpg <- dplyr::group_by(tmp, {{twin_col}})
  tmpg <- transmute(tmpg,
                    .id,
                    drop       = zygo_calc(drop, "drop", "pair"),
                    stranger   = zygo_calc(stranger, "stranger", "pair"),
                    dexterity  = zygo_calc(dexterity, "dexterity", "pair"),
                    eye        = zygo_calc(eye, "eye", "pair"),
                    belief     = zygo_calc(belief, "belief", "pair"),
                    voice      = NA_real_
  )
  tmp <- rows_patch(tmpg, tmpr, by = ".id")
  tmp <- ungroup(tmp)
  tmp <- select(tmp, -.id, -{{twin_col}})
  
  if(!is.null(prefix))
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
  
  if(keep_all) 
    tmp <- bind_cols(data, tmp)
  
  tmp
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
#' @return single value of calculated score based
#'         on recoded vector and multiplied with
#'         correct factor weight.
#' @export
#'
#' @examples
#' zygo_calc(c(1), type = "eye")
#' zygo_calc(c(1,3), type = "belief", n = "pair")
#' zygo_calc(c(4), type = "voice")
zygo_calc <- function(x, type, n = "single"){
  x <- zygo_recode(x, type)
  zygo_fct(x, type, n)
}

zygo_fct <- function(x, type, n = "single"){
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
    cli::cli_alert(sprintf(
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
                           "stranger",
                           "dexterity",
                           "voice",
                           "belief",
                           "eye"))
