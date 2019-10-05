
test_that("test edu4", {
  edu4 <- c("3", "High school", "1", 
            "University/University college (> 4 years)", "University/University college (> 4 years)", 
            "University/University college (< 4 years)")
  
  expect_equal(edu4_factorize(edu4),
               structure(c(3L, 2L, 1L, 4L, 4L, 3L), 
                         .Label = c("Primary school (9 years)", 
                                    "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"
                         ), 
                         class = c("lfactor", "factor"), 
                         llevels = c(9, 12, 16, 19)))
  
  expect_equal(edu4_to_years(edu4_factorize(edu4)),
               c(16, 12, 9, 19, 19, 16))
})

test_that("test edu10", {
  edu10 <- c(7,7,8,4,"Primary school (6 years)",5)
  
  expect_equal(edu10_factorize(edu10),
               structure(c(7L, 7L, 8L, 4L, 2L, 5L), 
                         .Label = c("Pre-school/No schooling", 
                                    "Primary school (6 years)", 
                                    "Secondary school (9 years)", 
                                    "High school (12 years)", 
                                    "High school diploma (13 years)", 
                                    "High school addition (14 years)", 
                                    "Lower level University/University college degree (16 years)",
                                    "Upper level University/University college (19 years)", 
                                    "Ph.D. (21 years)"
                         ), class = c("lfactor", "factor"), 
                         llevels = c(0, 6, 9, 12, 13, 14, 16, 19, 21))
  )
  
  expect_equal(edu10_to_years(edu10_factorize(edu10)),
               c(16, 16, 19, 12, 6, 13))
  
  expect_equal(edu10_reduce(edu10_factorize(edu10)),
               c("University/University college (< 4 years)", "University/University college (< 4 years)", 
                 "University/University college (> 4 years)", "High school", "Primary school (9 years)", 
                 "High school"))
})

test_that("test edu10", {
  test_dat <- data.frame(
    participant = structure(c(3L, 3L, NA, 4L, 4L, 4L, 4L, 4L, NA, 3L, 3L, 2L, NA, 3L, 3L, 3L, 3L, 3L, 3L, 4L), 
                            .Label = c("Primary school (9 years)", "High school", 
                                       "University/University college (< 4 years)", "University/University college (> 4 years)"), 
                            class = c("lfactor", "factor")), 
    mother = structure(c(NA, NA, NA, 4L, 4L, 4L, NA, NA, NA, 4L, 4L, 4L, 3L, 4L, 4L, 4L, 4L,NA, 4L, 4L),
                       .Label = c("Primary school (9 years)", "High school", 
                                  "University/University college (< 4 years)", "University/University college (> 4 years)"), 
                       class = c("lfactor", "factor")), 
    father = structure(c(NA, NA, NA, NA, 4L, 4L, 4L, 4L, 4L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, NA, NA, NA, NA), 
                       .Label = c("Primary school (9 years)", "High school", 
                                  "University/University college (< 4 years)", "University/University college (> 4 years)"),
                       class = c("lfactor", "factor")),
    stringsAsFactors = FALSE)
  
  
  compiled <- edu_compiled(data=test_dat, participant=participant, mother=mother, father=father)
  
  expect_equal(nrow(compiled), 20)
  expect_equal(ncol(compiled), 6)
  expect_equal(names(compiled), c("participant", "mother", "father", "Edu_Compiled_Code4", "Edu_Compiled_Years", 
                                  "Edu_Compiled_Source"))
  expect_equal(compiled$Edu_Compiled_Code4,
               structure(c(3L, 3L, NA, 4L, 4L, 4L, 4L, 4L, 4L, 3L, 3L, 2L, 3L, 
                           3L, 3L, 3L, 3L, 3L, 3L, 4L), 
                         .Label = c("Primary school (9 years)",  "High school", "University/University college (< 4 years)", "University/University college (> 4 years)"
                           ), 
                         class = c("lfactor", "factor"), 
                         llevels = c(9, 12, 16, 19)))
  
  expect_equal(compiled$Edu_Compiled_Years,
               c(16, 16, NA, 19, 19, 19, 19, 19, 19, 16, 16, 12, 16, 16, 16, 
                 16, 16, 16, 16, 19))
  
  expect_equal(compiled$Edu_Compiled_Source,
               c("Participant", "Participant", NA, "Participant", "Participant", 
                 "Participant", "Participant", "Participant", "Father", "Participant", 
                 "Participant", "Participant", "Mother", "Participant", "Participant", 
                 "Participant", "Participant", "Participant", "Participant", "Participant"
               ))
  
})
