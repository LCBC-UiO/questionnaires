
# Levels ----
#' Get education levels scheme
#' 
#' Keeping track of the different educational
#' coding schemes at LCBC can be tricky.
#' This formula contains the two current types
#' of coding schemas employed by LCBC.
#'
#' @param levels how many levels to return (either 4 or 9)
#' @details Specialized returns
#' \itemize{
#'  \item{edu_levels - }{returns named numeric vector for levels specified}
#'  \item{edu4_levels - }{returns named numeric vector for 4-levels scheme}
#'  \item{edu9_levels - }{returns named numeric vector for 9-levels scheme}
#' }
#' @return named numeric vector
#' @export
#' @family edu_functions
#' @examples
#' edu_levels(4)
#' edu_levels(9)
#' 
#' edu4_levels()
#' edu9_levels()
edu_levels <- function(levels = 4){
  
  levs <- list(
    `4` = c("Primary school (9 years)" = 9,
            "High school" = 12,
            "University/University college (< 4 years)" = 16,
            "University/University college (> 4 years)" = 19),
    `9` =  c("Pre-school/No schooling" = 0,
             "Primary school (6 years)" = 6,
             "Secondary school (9 years)" = 9,
             "High school (12 years)" = 12,
             "High school diploma (13 years)" = 13,
             "High school addition (14 years)" = 14,
             "Lower level University/University college degree (16 years)" = 16,
             "Upper level University/University college (19 years)" = 19,
             "Ph.D. (21 years)" = 21)
  )
  
  if(as.character(levels) %in% names(levs)){
    return(levs[[as.character(levels)]])
  }
  stop("There is no stored scheme for levels == '", levels, "'",
       call. = FALSE)
}

#' Recode new 9 levels into old
#' 
#' New nettskjema data requires codebook to
#' not have special characters, and as such the old and
#' new coding scheme does not fit. This function turns new
#' coding scheme into the old, wanted one
#'
#' @param x character vector of old scheme
#' @param names logical. toggle return of names rather than numbers
#'
#' @return character
#' @export
#'
#' @examples
#' eds <- c(NA, "UnderGrad_BA", "HighSchool_Initial", "PostGrad_MA", 
#'    "PostGrad_PhD", "HighSchool", "Junior-HighSchool", "HighSchool_addition")
#' edu_recode(eds) 
#' 
#' eds <- c(1,5,8,2,6,9,1,10)
#' edu_recode(eds, names = FALSE) 
edu_recode <- function(x, names = TRUE){
  levs <- 1:10
  if(names){
    levs <- c("Pre-school-NoSchool", "PrimarySchool", "Junior-HighSchool", 
              "HighSchool_Initial", "HighSchool", "HighSchool_addition", "UnderGrad_BA", 
              "PostGrad_MA", "PostGrad_PhD")
  }
  names(levs) <- c("Pre-school/No schooling", "Primary school (6 years)", "Secondary school (9 years)", 
                   "High school (12 years)", "High school diploma (13 years)", "High school addition (14 years)", 
                   "Lower level University/University college degree (16 years)", 
                   "Upper level University/University college (19 years)", "Ph.D. (21 years)"
  )
  names(levs[match(x, levs)])
}

#' @rdname edu_levels 
#' @export
edu4_levels <- function(){
  edu_levels(4)
}

#' @rdname edu_levels 
#' @export
edu9_levels <- function(){
  edu_levels(9)
}

#' Alter levels to name
#' 
#' Change educational coded levels
#' to names of the levels
#'
#' @param x vector containing levels
#' @param levels numeric of number of levels (4 or 10)
#' @details Specialized returns
#' \itemize{
#'  \item{edu_levels2name - }{transforms levels to names for levels specified}
#'  \item{edu4_levels2name - }{transforms levels to names for 4-levels scheme}
#'  \item{edu9_levels2name - }{transforms levels to names for 9-levels scheme}
#' }
#' @return character vector
#' @family edu_functions
#' @export
#' @examples 
#' edu4 <- c(9, 9, 16, 19)
#' edu_levels2name(edu4, 4)
#' # does the same as
#' edu4_levels2name(edu4)
#' 
#' edu9 <- c(0, 6, 21, 16)
#' edu_levels2name(edu9, 9)
#' # does the same as
#' edu9_levels2name(edu9)
edu_levels2name <- function(x, levels){
  t <- factor(x, levels = unname(edu_levels(levels)))
  levels(t) <- names(edu_levels(levels))
  as.character(t)
}

#' @rdname edu_levels2name 
#' @export
edu4_levels2name <- function(x){
  edu_levels2name(x, 4)
}

#' @rdname edu_levels2name 
#' @export
edu9_levels2name <- function(x){
  edu_levels2name(x, 9)
}

# Maps ----
#' Create a mapped table for conversion
#' 
#' Converting from a high-level educational
#' coding to a lower level one is cumbersome.
#' This function bases it self in any coding
#' scheme specified in \code{\link{edu_levels}} 
#' and tries 
#' creating a conversion table between two
#' specified schemas.
#' 
#' @param from schema levels to convert from
#' @param to shcema levels to convtert to
#' @export
#' @details Specialized returns
#' \itemize{
#'  \item{edu_map - }{returns a data.frame of two named vectors}
#'  \item{edu_map_chr - }{returns a data.frame with two character vectors}
#'  \item{edu_map_num - }{returns a data.frame with two numeric vectors}
#' }
#' @importFrom stats setNames na.omit
#' @importFrom dplyr mutate filter group_by select distinct
edu_map <- function(from = 9, to = 4){
  
  toi <- to
  fromi <- from
  
  all <- expand.grid(list(
    from = edu_levels(from),
    to = edu_levels(to)
  ))
  
  # If the minimum in from is smaller
  # than min in to: cannot map, set 999 to return NA later
  if(min(all$from) < min(all$to)){
    all[which(min(all$from) < min(all$to)), "to"] <- 999  
  }
  
  comps <- all |> 
    mutate(x = from - to) |> 
    filter(x >= 0 | x == -999) |> 
    group_by(from) |> 
    mutate(y = min(x),
           to = ifelse(x == y, to, NA)) |> 
    select(-x,-y) |> 
    distinct() |> 
    na.omit() |> 
    ungroup() |> 
    mutate(to = ifelse(to == 999, NA, to))
  
  mutate(comps, 
         to = setNames(to, edu_levels2name(to, toi)),
         from = setNames(from, edu_levels2name(from, fromi))) 
}

#' @rdname edu_map 
#' @importFrom dplyr mutate
#' @export
edu_map_chr <- function(from = 9, to = 4){
  mutate(edu_map(from, to),
         to = names(to),
         from = names(from))
}

#' @rdname edu_map 
#' @importFrom dplyr mutate
#' @export
edu_map_num <- function(from = 9, to = 4){
  mutate(edu_map(from, to),
         to = unname(to),
         from = unname(from))
}

# Factorise ----
#' Create factor from education vector
#' 
#' Will convert even a mixed character
#' vector (combining numbers and text)
#' of education levels 10 and 4 to a 
#' factor.
#'
#' @param x character vector
#' @param levels levels returned from the edu_levels() function
#' @details Specialized returns
#' \itemize{
#'  \item{edu_factorise - }{with option to choose number of levels}
#'  \item{edu4_factorise - }{directly transform vector coded in 4-level scheme}
#'  \item{edu9_factorise - }{directly transform vector coded in 9-levels scheme}
#' }
#' @return factor
#' @export
#' @family edu_functions
#' @importFrom lfactors lfactor
#' @examples 
#' 
#' edu9 <- c("7", "7", "8", NA, "Primary school (6 years)", "5", "9")
#' edu_factorise(edu9, 9)
#' edu9_factorise(edu9)
edu_factorise <- function(x, levels){
  levs <- edu_levels(levels)
  t <- x
  
  # Recode all to integer
  k <- suppressWarnings(as.numeric(t))
  if(!is.numeric(t)){
    for(i in which(is.na(k) & str_count(t) > 1)){
      t[i] <- which(names(levs) %in% t[i])
    }
    t <- as.numeric(t)
  }
  
  t <- sapply(1:length(t), function(x) levs[t[x]])
  
  lfactor(as.numeric(t),
          levels = levs,
          labels = names(levs),
          ordered = TRUE
  )
}

#' @rdname edu_factorise
#' @export
edu4_factorise <- function(x){
  edu_factorise(x, 4)
}

#' @rdname edu_factorise
#' @export
edu9_factorise <- function(x){
  edu_factorise(x, 9)
}

# Reduce ----
#' Reduce education categories
#' 
#' These functions will aid in converting
#' one education scheme into another.
#' While you may attempt to go from a low
#' level to a high (from 4 to 9), there is 
#' no way to actually do that in a consistent
#' way that will correctly reflect the 
#' underlying data.
#' 
#' Always go from a higher level scheme to
#' a lower one (currently from 9 to 4 only)
#' 
#' @details Specialized returns
#' \itemize{
#'  \item{edu_reduce - }{reduce with own to and from specification}
#'  \item{edu9_reduce - }{directly reduce from 9 to 4}
#' }
#' @inheritParams edu_factorise
#' @param to factor level to transform to
#' @param from factor level to transform from
#' @family edu_functions
#' @importFrom dplyr left_join
#' @return factor
#' @export
#' @examples
#' edu9 <- c("7", "7", "8", NA, "Primary school (6 years)", "5", "9")
#' edu_reduce(edu9, 9, 4)
#' edu9_reduce(edu9)
edu_reduce <- function(x, from, to){
  
  if(!is.factor(x) & 
     all(levels(x) != names(edu_levels(from)))
  ){
    x <- edu_factorise(x, from)
  }
  
  edu <- edu_map_chr(from, to)
  
  t <- data.frame(from = as.character(x), stringsAsFactors = FALSE)
  t <- left_join(t, edu, by ="from")
  
  edu_factorise(t$to, to)
}

#' @rdname edu_reduce
#' @export
edu9_reduce <- function(x, to = 4){
  edu_reduce(x, 9, to)
}


# Edu to year ----
#' Turn education data to years
#' 
#' @details Specialized returns
#' \itemize{
#'  \item{edu_to_years - }{Alter education to years specifying number of levels}
#'  \item{edu4_to_years - }{directly alter 4-level coded education to years }
#'  \item{edu9_to_years - }{directly alter 9-level coded education to years }
#' }
#' @inheritParams edu_factorise 
#' @family edu_functions
#' @return vector of integers
#' @export
#' @examples 
#' 
#' edu4 <- c("3", "High school", "1", NA, 
#'           "University/University college (> 4 years)", 
#'            NA, "University/University college (< 4 years)")
#'
#' edu_to_years(edu4, 4)
#' edu4_to_years(edu4)
#' 
#' edu9 <- c("7", "7", "8", NA, "Primary school (6 years)", "5", "9")
#' edu_to_years(edu9, 9)
#' edu9_to_years(edu9)

edu_to_years <- function(x, levels){
  
  t <- if(is.factor(x) & length(levels(x)) == levels){
    x
  }else{
    edu_factorise(as.character(x), levels)
  }
  
  as.numeric(t)
}

#' @rdname edu_to_years
#' @export
edu4_to_years <- function(x){
  edu_to_years(x, 4)
}

#' @rdname edu_to_years
#' @export
edu9_to_years <- function(x){
  edu_to_years(x, 9)
}

# Composites ----
#' Fill inn Education in MOAS
#' 
#' Using existing data in the MOAS, fills in gaps, 
#' converts from on type of coding to another etc.
#'
#' @param data MOAS-like data
#' @param edu4 unquoted column containing Education coded in 4 categories
#' @param edu9 unquoted column containing Education coded in 4 categories
#' @param edu_years unquoted column containing Education in years to highest completed
#' @template keep_all 
#' @template prefix
#'
#' @importFrom dplyr case_when transmute
#' @family edu_functions
#' @return a data.frame
#' @export
#' @examples 
#' 
#' edu <- data.frame(
#'     edu4 = c("3", "High school", 1, NA,
#'          "University/University college (> 4 years)", NA, 
#'           "University/University college (< 4 years)"),
#'     edu9 = c(7,7,8,NA,"Primary school (6 years)",5, 9),
#'     edu_years = c(NA, 12, 9, NA, 19, 19, NA),
#'     mother = c("3", "High school", 1, NA,
#'                "University/University college (> 4 years)",
#'                "University/University college (> 4 years)", 
#'                "University/University college (< 4 years)"),
#'     father = c(7,7,8,4,"Primary school (6 years)",5, 10),
#'     stringsAsFactors = FALSE
#'     )
#'  
#'  edu_compute(edu,
#'              edu4 = edu4,
#'              edu9 = edu9, 
#'              edu_years = edu_years)
edu_compute <- function(data, 
                        edu4 = edu_coded4,
                        edu9 = edu_coded10,
                        edu_years = edu_years,
                        prefix = "edu_",
                        keep_all = TRUE){
  
  tmp <- transmute(data,
                   coded9 = edu9_factorise( {{edu9}} ),
                   coded4 = ifelse(is.na( {{edu4}} ), 
                                   edu9_reduce( {{edu9}} ),
                                   {{edu4}} ),
                   coded4 = edu4_factorise( {{edu4}}),
                   years = case_when(
                     !is.na({{edu_years}}) ~ round({{edu_years}}, 0),
                     !is.na({{edu9}}) ~ edu9_to_years({{edu9}}),
                     !is.na({{edu4}}) ~ edu4_to_years({{edu4}})
                   )
  )
  if(!is.null(prefix)){
    tmp <- rename_all(tmp, ~paste0(prefix, .x))
  }
  if(keep_all){
    tmp <- bind_cols(data, tmp)
  }
  tmp
}

#' Compile education across sources
#' 
#' Compiles education from participant, mother or father depending
#' on source availability. Made for ease of testing and reporting education SES
#' of family
#'
#' @param data MOAS-like data.frame
#' @param participant unquoted column of 4 category education for participant
#' @param mother unquoted column of 4 category education for participant's mother
#' @param father unquoted column of 4 category education for participant's father
#'
#' @return dataframe with three new columns
#' @export
#' @family edu_functions
#' @importFrom dplyr mutate case_when
#' @examples 
#' 
#' edu <- data.frame(
#'     edu4 = c("3", "High school", 1, NA,
#'          "University/University college (> 4 years)", NA, 
#'           "University/University college (< 4 years)"),
#'     edu9 = c(7,7,8,NA,"Primary school (6 years)",5, 9),
#'     edu_years = c(NA, 12, 9, NA, 19, 19, NA),
#'     mother = c("3", "High school", 1, NA,
#'                "University/University college (> 4 years)",
#'                "University/University college (> 4 years)", 
#'                "University/University college (< 4 years)"),
#'     father = c(7,7,8,4,"Primary school (6 years)",5, 10),
#'     stringsAsFactors = FALSE
#'     )
#'  
#'  library(dplyr)
#'  edu |> 
#'      mutate(
#'         mother = ifelse(mother == "3", NA, mother),
#'         mother = edu4_factorise(mother),
#'         father = edu9_reduce(edu9_factorise(father))
#'      ) |> 
#'      edu_compile(
#'         participant = edu4,
#'         mother = mother,
#'         father = father
#'         )
edu_compile <- function(data,
                        participant,
                        mother,
                        father){
  
  mutate(data,
         edu_compiled_code4 = edu4_factorise(case_when(
           !is.na( {{participant}} ) ~ as.character( {{participant}} ),
           !is.na( {{mother}} ) ~ as.character( {{mother}} ),
           !is.na( {{father}} ) ~ as.character( {{father}} ))),
         edu_compiled_years = as.numeric(edu_compiled_code4),
         edu_compiled_source = case_when(
           !is.na( {{participant}} ) ~ "Participant",
           !is.na( {{mother}} ) ~ "Mother",
           !is.na( {{father}} ) ~ "Father")
  )
}

if(getRversion() >= "2.15.1")  
  utils::globalVariables(c("edu_coded4", 
                           "edu_coded10", 
                           "edu_years",
                           "edu_compiled_code4", 
                           "x", "y"))
