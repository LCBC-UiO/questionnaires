
edu <- data.frame(
  edu4 = c("3", "High school", 1, NA,
           "University/University college (> 4 years)", NA, 
           "University/University college (< 4 years)"),
  edu9 = c(7,7,8,NA,"Primary school (6 years)",5, 9),
  edu_years = c(NA, 12, 9, NA, 19, 19, NA),
  mother = c("3", "High school", 1, NA,
             "University/University college (> 4 years)", "University/University college (> 4 years)", 
             "University/University college (< 4 years)"),
  father = c(7,7,8,4,"Primary school (6 years)",5, 10),
  stringsAsFactors = FALSE
)

# Levels ----
test_that("edu_levels works",{
  expect_equal(edu_levels(4),
               c(`Primary school (9 years)` = 9, 
                 `High school` = 12, 
                 `University/University college (< 4 years)` = 16, 
                 `University/University college (> 4 years)` = 19)
  )
  
  expect_equal(edu_levels(4),
               edu4_levels())
  
  expect_equal(edu_levels(9),
               c(`Pre-school/No schooling` = 0, 
                 `Primary school (6 years)` = 6, 
                 `Secondary school (9 years)` = 9, 
                 `High school (12 years)` = 12, 
                 `High school diploma (13 years)` = 13,
                 `High school addition (14 years)` = 14, 
                 `Lower level University/University college degree (16 years)` = 16, 
                 `Upper level University/University college (19 years)` = 19, 
                 `Ph.D. (21 years)` = 21)
  )
  
  expect_equal(edu_levels(9),
               edu9_levels())
  
  expect_error(edu_levels(1), 
               "no stored scheme for levels == '1'")  
  
})

test_that("edu_levels2name works", {
  
  expect_equal(
    edu_levels2name(c(9,9,16,19), 4),
    c("Primary school (9 years)", "Primary school (9 years)", 
      "University/University college (< 4 years)", 
      "University/University college (> 4 years)")
  )
  
  expect_equal(
    edu_levels2name(c(9,9,16,19), 4),
    edu4_levels2name(c(9,9,16,19))
  )
  
  expect_equal(
    edu_levels2name(c(0, 6, 21, 16), 9) ,
    c("Pre-school/No schooling", "Primary school (6 years)", 
      "Ph.D. (21 years)", 
      "Lower level University/University college degree (16 years)")
  )
  
  expect_equal(
    edu_levels2name(c(0, 6, 21, 16), 9),
    edu9_levels2name(c(0, 6, 21, 16))
  )
    
  expect_error(edu_levels2name(c(0, 6, 21, 16), 1),
               "There is no stored scheme for levels == '1'")
  
  
})

# Maps ----
test_that("edu_map works", {
  
  k <- edu_map()
  
  expect_equal(nrow(k), 8)
  expect_equal(nrow(k), 8)
  expect_equal(names(k), c("from", "to"))

  expect_equal(k$to, 
               setNames(c(NA, 9, 12, 12, 12, 16, 19, 19), 
                         c(NA, "Primary school (9 years)", "High school", "High school", 
                           "High school", "University/University college (< 4 years)",
                           "University/University college (> 4 years)", "University/University college (> 4 years)")
                        )
  )

  expect_equal(k$from,
               setNames(c(0,9,12,13,14,16,19,21),
                        c("Pre-school/No schooling", 
                          "Secondary school (9 years)",
                          "High school (12 years)",
                          "High school diploma (13 years)",
                          "High school addition (14 years)",
                          "Lower level University/University college degree (16 years)",
                          "Upper level University/University college (19 years)",
                          "Ph.D. (21 years)")))
  

  expect_equal(
    apply(k, 2, class),
    c(from = "numeric", to = "numeric")
  )
  
  expect_equal(edu_map_chr(),
               dplyr::tibble(from = c("Pre-school/No schooling", 
                                   "Secondary school (9 years)", 
                                   
                                   "High school (12 years)", 
                                   "High school diploma (13 years)", 
                                   "High school addition (14 years)", 
                                   "Lower level University/University college degree (16 years)",
                                   "Upper level University/University college (19 years)", 
                                   "Ph.D. (21 years)"), 
                          to = c(NA, 
                                 "Primary school (9 years)", 
                                 "High school", "High school", 
                                 "High school", 
                                 "University/University college (< 4 years)", 
                                 "University/University college (> 4 years)", 
                                 "University/University college (> 4 years)"))
  )
  
  expect_equal(
    apply(edu_map_chr(),2, class),
    c(from = "character", to = "character")
  )
  
  expect_equal(edu_map_num(),
               dplyr::tibble(
                 from = c(0, 9, 12, 13, 14, 16, 19, 21), 
                 to = c(NA, 9, 12, 12, 12, 16, 19, 19))
               )
  
  expect_equal(
    apply(edu_map_num(), 2, class),
    c(from = "numeric", to = "numeric")
  )
})
  
# factorise ----
test_that("edu_factorise works",{
  
  k1 <- edu4_factorise(edu$edu4)
  k2 <- edu_factorise(edu$edu4, 4)
  
  expect_equal(k1, k2)
  expect_equal(k1, 
               structure(c(3L, 2L, 1L, NA, 4L, NA, 3L), 
                         .Label = c("Primary school (9 years)", 
                                    "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(`Primary school (9 years)` = 9,
                                     `High school` = 12, 
                                     `University/University college (< 4 years)` = 16,
                                     `University/University college (> 4 years)` = 19))
  )
  
  k1 <- edu9_factorise(edu$edu9)
  k2 <- edu_factorise(edu$edu9, 9)
  expect_equal(k1, k2)
  expect_equal(k1, 
               structure(c(7L, 7L, 8L, NA, 2L, 5L, 9L), 
                         .Label = c("Pre-school/No schooling", 
                                    "Primary school (6 years)",
                                    "Secondary school (9 years)", "High school (12 years)", 
                                    "High school diploma (13 years)", 
                                    "High school addition (14 years)", 
                                    "Lower level University/University college degree (16 years)", 
                                    "Upper level University/University college (19 years)", "Ph.D. (21 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(`Pre-school/No schooling` = 0, 
                                     `Primary school (6 years)` = 6, 
                                     `Secondary school (9 years)` = 9, 
                                     `High school (12 years)` = 12, 
                                     `High school diploma (13 years)` = 13, 
                                     `High school addition (14 years)` = 14, 
                                     `Lower level University/University college degree (16 years)` = 16, 
                                     `Upper level University/University college (19 years)` = 19, 
                                     `Ph.D. (21 years)` = 21))
  )
  
})


# Reduce ----
test_that("check that edu_reduce works",{
  
  k1 <- edu_reduce(edu$edu9, 9, 4)
  k2 <- edu9_reduce(edu$edu9)

  expect_equal(k1, k2)
  expect_equal(k1, 
               structure(c(3L, 3L, 4L, NA, NA, 2L, 4L), 
                         .Label = c("Primary school (9 years)",
                                    "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(`Primary school (9 years)` = 9, 
                                     `High school` = 12, 
                                     `University/University college (< 4 years)` = 16,
                                     `University/University college (> 4 years)` = 19))
  )
  
  k2 <- edu9_reduce(edu$edu9)
  expect_equal(k1, k2)
})


# Edu to year ----
test_that("test edu_to_year", {
  
  expect_equal(edu_to_years(edu$edu4, 4),
               c(16, 12, 9, NA, 19, NA, 16))
  
  expect_equal(edu4_to_years(edu$edu4),
               c(16, 12, 9, NA, 19, NA, 16))
  
  expect_equal(edu_to_years(edu$edu9, 9),
               c(16, 16, 19, NA, 6, 13, 21))
  
  expect_equal(edu9_to_years(edu$edu9),
               c(16, 16, 19, NA, 6, 13, 21))
})


# composites ----

test_that("test edu_compute", {
  
  edu2 <- edu_compute(edu,
                      edu4 = edu4, 
                      edu9 = edu9, 
                      edu_years = edu_years,
                      prefix = "edu2_")
  
  expect_equal(nrow(edu2), 7)
  expect_equal(ncol(edu2), 8)
  expect_equal(edu2$edu2_years, c(16, 12, 9, NA, 19, 19, 21))
  
  edu2 <- edu_compute(edu,
                      edu4 = edu4, 
                      edu9 = edu9, 
                      edu_years = edu_years, keep_all = FALSE)
  expect_equal(nrow(edu2), 7)
  expect_equal(ncol(edu2), 3)
  expect_equal(edu2$edu_years, c(16, 12, 9, NA, 19, 19, 21))
})


test_that("test edu_compile", {
  
  compiled <- edu_compile(data = edu, 
                          participant = edu4, 
                          mother = edu4_factorise(mother), 
                          father = father
  )
  
  expect_equal(nrow(compiled), 7)
  expect_equal(ncol(compiled), 8)
  expect_equal(names(compiled), c("edu4", "edu9", "edu_years", "mother", "father", "edu_compiled_code4", "edu_compiled_years", 
                                  "edu_compiled_source"))
  expect_equal(compiled$edu_compiled_code4,
               structure(c(3L, 2L, 1L, 4L, 4L, 4L, 3L), 
                         .Label = c("Primary school (9 years)", 
                                    "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(`Primary school (9 years)` = 9,
                                     `High school` = 12, 
                                     `University/University college (< 4 years)` = 16,
                                     `University/University college (> 4 years)` = 19)
                         )
  )
  
  expect_equal(compiled$edu_compiled_years,
               c(16, 12, 9, 19, 19, 19, 16))
  
  expect_equal(compiled$edu_compiled_source,
               c("Participant", "Participant", "Participant", "Father", "Participant", 
                 "Mother", "Participant"))
  
  # reduce edu9 correctly for father
  compiled <- edu |> 
    mutate(
      mother = ifelse(mother == "3", NA, mother),
      mother = edu4_factorise(mother),
      father = edu9_reduce(edu9_factorise(father))
    ) |> 
    edu_compile(
      participant = edu4,
      mother = mother,
      father = father
    )
  
  expect_equal(compiled$edu_compiled_code4,
               structure(c(3L, 2L, 1L, 2L, 4L, 4L, 3L), 
                         .Label = c("Primary school (9 years)", 
                                    "High school", 
                                    "University/University college (< 4 years)", 
                                    "University/University college (> 4 years)"), 
                         class = c("lfactor", "factor"), 
                         llevels = c(`Primary school (9 years)` = 9, 
                                     `High school` = 12, 
                                     `University/University college (< 4 years)` = 16, 
                                     `University/University college (> 4 years)` = 19))
  )
  
  expect_equal(compiled$edu_compiled_years,
               c(16, 12, 9, 12, 19, 19, 16))
  
  expect_equal(compiled$edu_compiled_source,
               c("Participant", "Participant", "Participant", "Father", "Participant", 
                 "Mother", "Participant"))
  
  
})
