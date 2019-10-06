#' Factorise 4 category education data
#' 
#' @param x Vector of 4 category education data
#' @family education_functions
#' @importFrom lfactors lfactor
#' @importFrom stringr str_count
#' @export
edu4_factorize <- function(x){
  
  edu <- c("Primary school (9 years)",
           "High school",
           "University/University college (< 4 years)",
           "University/University college (> 4 years)")
  edulev <- c(9, 12, 16, 19)
  
  t <- x
  
  # Recode all to integer
  for(i in which(stringr::str_count(t) > 1)){
    t[i] <- which(edu %in% t[i])
  }
  
  t <- as.numeric(t)
  
  for(i in 1:length(t)){
    t[i] <- edulev[t[i]]
  }
  
  lfactors::lfactor(as.numeric(t),
                    levels = edulev,
                    labels = edu
  )
}

#' Turn 4 category education data to years
#'
#' @inheritParams edu4_factorize 
#' @family education_functions
#' @return vector of integers
#' @export
edu4_to_years <- function(x){
  
  t <- if(is.factor(x) && length(levels(x)) == 4){
    x
  }else{
    edu4_factorize(x)
  }
  
  as.numeric(t)
}

#' Factorize vector of 10 category education data.
#' 
#' @param x Vector of 10 category education data
#' @family education_functions
#' @importFrom lfactors lfactor
#' @importFrom stringr str_count
#' @export
edu10_factorize <- function(x){
  
  edu <- c("Pre-school/No schooling",
           "Primary school (6 years)",
           "Secondary school (9 years)",
           "High school (12 years)",
           "High school diploma (13 years)",
           "High school addition (14 years)",
           "Lower level University/University college degree (16 years)",
           "Upper level University/University college (19 years)",
           "Ph.D. (21 years)")
  
  edulev <- c(0, 6, 9, 12:14, 16, 19, 21)
  
  t <- x
  
  # Recode all to integer
  for(i in which(stringr::str_count(t) > 1)){
    if(t[i] == 10){
      t[i] <- NA
    } else{
      t[i] <- which(edu %in% t[i])
    }
  }
  
  t <- as.numeric(t)
  
  for(i in 1:length(t)){
    t[i] <- edulev[t[i]]
  }
  
  lfactors::lfactor(as.numeric(t),
                    levels = edulev,
                    labels = edu
  )
}

#' Reduce 10 category education to 4
#' 
#' @inheritParams edu10_factorize
#' @family education_functions
#' @importFrom dplyr left_join
#' @export
edu10_reduce <- function(x){
  edu <- data.frame(edu10=c("Pre-school/No schooling",
                            "Primary school (6 years)",
                            "Secondary school (9 years)",
                            "High school (12 years)",
                            "High school diploma (13 years)",
                            "High school addition (14 years)",
                            "Lower level University/University college degree (16 years)",
                            "Upper level University/University college (19 years)",
                            "Ph.D. (21 years)"),
                    edu4=c(NA, 
                           "Primary school (9 years)", 
                           "Primary school (9 years)",
                           "High school",
                           "High school",
                           "High school",
                           "University/University college (< 4 years)",
                           "University/University college (> 4 years)",
                           "University/University college (> 4 years)"
                    ),
                    stringsAsFactors=FALSE
  )
  
  t <- data.frame(edu10 = as.character(x), stringsAsFactors = FALSE)
  t <- dplyr::left_join(t, edu, by ="edu10")
  
  t$edu4
}

#' Alter factorized education in 10 categories to years
#' 
#' @inheritParams edu10_factorize
#' @family education_functions
#' @return integer vector
#' @export
edu10_to_years <- function(x){
  
  t <- if(is.factor(x) && length(levels(x)) == 9){
    x
  }else{
    edu10_factorize(x)
  }
  
  as.numeric(t)
}

#' Fill inn Education in MOAS
#' 
#' Using existing data in the MOAS, fills in gaps, 
#' converts from on type of coding to another etc.
#'
#' @param data MOAS-like data
#' @param edu4 unquoted column containing Education coded in 4 categories
#' @param edu10 unquoted column containing Education coded in 4 categories
#' @param edu_years unquoted column containing Education in years to highest completed
#' @param keep_all  logical, append to data.frame
#'
#' @importFrom dplyr select starts_with
#' @importFrom data.table ':='
#' @family education_functions
#' @return a data.frame
#' @export
education_compute <- function(data, 
                              edu4 = Edu_Coded4,
                              edu10 = Edu_Coded10,
                              edu_years = Edu_Years,
                              keep_all = TRUE){
  
  tmp <- mutate(data,
                 {{edu10}} := edu10_factorize( {{edu10}} ),
                 {{edu4 }} := ifelse(is.na( {{edu4}} ), 
                                 edu10_reduce( {{edu10}} ),
                                  {{edu4}} ),
                 {{edu4}}  := edu4_factorize( {{edu4}} ),
                {{edu_years}}  := ifelse(is.na( {{edu_years}} ) |  {{edu_years}}  < 6,
                                      as.numeric( {{edu10}} ),
                                       {{edu_years}} ),
                 {{edu_years}}  := ifelse(is.na( {{edu_years}} ),
                                      edu4_to_years( {{edu4}} ),
                                       {{edu_years}} )
  )
  
  if(keep_all){
    tmp
  }else{
    dplyr::select(tmp, dplyr::starts_with("Edu"))
  }
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
#' @family education_functions
edu_compile <- function(data,
                         participant,
                         mother,
                         father){
  
  dplyr::mutate(data,
         Edu_Compiled_Code4 = edu4_factorize(dplyr::case_when(
           !is.na( {{participant}} ) ~ as.character( {{participant}} ),
           !is.na( {{mother}} ) ~ as.character( {{mother}} ),
           !is.na( {{father}} ) ~ as.character( {{father}} ))),
         Edu_Compiled_Years = as.numeric(Edu_Compiled_Code4),
         Edu_Compiled_Source = dplyr::case_when(
           !is.na( {{participant}} ) ~ "Participant",
           !is.na( {{mother}} ) ~ "Mother",
           !is.na( {{father}} ) ~ "Father")
  )
}

if(getRversion() >= "2.15.1")  utils::globalVariables(c("Edu_Coded4", "Edu_Coded10", "Edu_Years",
                                                        "Edu_Compiled_Code4"))
