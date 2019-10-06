
edu <- data.frame(
  edu4 = c("3", "High school", 1, NA,
           "University/University college (> 4 years)", NA, 
           "University/University college (< 4 years)"),
  edu10 = c(7,7,8,4,"Primary school (6 years)",5, 10),
  edu_years = c(NA, 12, 9, NA, 19, 19, NA),
  mother = c("3", "High school", 1, NA,
             "University/University college (> 4 years)", "University/University college (> 4 years)", 
             "University/University college (< 4 years)"),
  father = c(7,7,8,4,"Primary school (6 years)",5, 10),
  stringsAsFactors = FALSE
)

test_that("test edu$edu4", {
  
  expect_equal(edu4_factorize(edu$edu4),
               structure(c(3L, 2L, 1L, NA, 4L, NA, 3L), 
                         .Label = c("Primary school (9 years)", 
                                    "High school", "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(9, 12, 16, 19))
  )
  
  expect_equal(edu4_to_years(edu4_factorize(edu$edu4)),
               c(16, 12, 9, NA, 19, NA, 16))
  
  expect_equal(edu4_to_years(edu$edu4),
               c(16, 12, 9, NA, 19, NA, 16))
})

test_that("test edu10", {
  
  expect_equal(edu10_factorize(edu$edu10),
               structure(c(7L, 7L, 8L, 4L, 2L, 5L, NA), 
                         .Label = c("Pre-school/No schooling", 
                                    "Primary school (6 years)",
                                    "Secondary school (9 years)", 
                                    "High school (12 years)", 
                                    "High school diploma (13 years)", 
                                    "High school addition (14 years)", 
                                    "Lower level University/University college degree (16 years)", 
                                    "Upper level University/University college (19 years)", 
                                    "Ph.D. (21 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(0, 6, 9, 12, 13, 
                                     14, 16, 19, 21))
  )
  
  expect_equal(edu10_to_years(edu10_factorize(edu$edu10)),
               c(16, 16, 19, 12, 6, 13, NA))
  
  expect_equal(edu10_to_years(edu$edu10),
               c(16, 16, 19, 12, 6, 13, NA))
  
  expect_equal(edu10_reduce(edu10_factorize(edu$edu10)),
               c("University/University college (< 4 years)", "University/University college (< 4 years)", 
                 "University/University college (> 4 years)", "High school", "Primary school (9 years)", 
                 "High school", NA_character_))
  
})

test_that("test edu_compile", {
  
  compiled <- edu_compile(data=edu, 
                          participant=edu4, 
                          mother=edu4_factorize(mother), 
                          father=edu10_reduce(father)
  )
  
  expect_equal(nrow(compiled), 7)
  expect_equal(ncol(compiled), 8)
  expect_equal(names(compiled), c("edu4", "edu10", "edu_years", "mother", "father", "Edu_Compiled_Code4", "Edu_Compiled_Years", 
                                  "Edu_Compiled_Source"))
  expect_equal(compiled$Edu_Compiled_Code4,
               structure(c(3L, 2L, 1L, NA, 4L, 4L, 3L), 
                         .Label = c("Primary school (9 years)", "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"),
                         class = c("lfactor", "factor"), 
                         llevels = c(9, 12, 16, 19))
  )
  
  expect_equal(compiled$Edu_Compiled_Years,
               c(16, 12, 9, NA, 19, 19, 16))
  
  expect_equal(compiled$Edu_Compiled_Source,
               c("Participant", "Participant", "Participant", NA, "Participant", 
                 "Mother", "Participant"))
  
  # reduce edu10 correctly for father
  compiled <- edu %>% 
    mutate(
      mother = ifelse(mother == "3", NA, mother),
      mother = edu4_factorize(mother),
      father=edu10_reduce(edu10_factorize(father))
    ) %>% 
    edu_compile(
      participant = edu4,
      mother = mother,
      father = father
    )
  
  expect_equal(compiled$Edu_Compiled_Code4,
               structure(c(3L, 2L, 1L, 2L, 4L, 4L, 3L), 
                         .Label = c("Primary school (9 years)", "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"),
                         class = c("lfactor", "factor"), 
                         llevels = c(9, 12, 16, 19))
  )
  
  expect_equal(compiled$Edu_Compiled_Years,
               c(16, 12, 9, 12, 19, 19, 16))
  
  expect_equal(compiled$Edu_Compiled_Source,
               c("Participant", "Participant", "Participant", "Father", "Participant", 
                 "Mother", "Participant"))
  
  
})

test_that("test education_compute", {
  
  edu2 <- education_compute(edu,
                    edu4 = edu4, 
                    edu10 = edu10, 
                    edu_years = edu_years)
  
  expect_equal(nrow(edu2), 7)
  expect_equal(ncol(edu2), 5)
  expect_equal(edu2$edu_years, c(16, 12, 9, 12, 19, 19, 16))
  
  edu2 <- education_compute(edu,
                            edu4 = edu4, 
                            edu10 = edu10, 
                            edu_years = edu_years, keep_all = FALSE)
  expect_equal(nrow(edu2), 7)
  expect_equal(ncol(edu2), 3)
  expect_equal(edu2$edu_years, c(16, 12, 9, 12, 19, 19, 16))
})
