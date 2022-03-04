ehi <- read.delim(test_path("data/ehi.tsv"))

test_that("Check component calculations", {
  
  expect_equal(ehi_compute_lq(ehi),
               c(73.3333333333333, 11.1111111111111, 85.7142857142857, 100, 
                 100, 100, 100, 100, 100, 89.4736842105263))
  
  expect_equal(ehi_factorise_lq(ehi_compute_lq(ehi)),
               structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
                         .Label = c("left", 
                                    "right"), 
                         class = "factor"))
  
  expect_equal(ehi_factorise_lqa(ehi_compute_lq(ehi)),
               structure(c(3L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
                         .Label = c("left", "ambidexter", "right"), class = "factor"))
  
  expect_equal(ehi_factorise_lqa(c(NA, 11, 85, 40, 100, -41, 50, 100, 100, 89), min = -40, max = 40),
               structure(c(NA, 2L, 3L, 2L, 3L, 1L, 3L, 3L, 3L, 3L), 
                         .Label = c("left", "ambidexter", "right"), class = "factor"))
  
  expect_equal(as.character(ehi_factorise_lq(ehi_compute_lq(ehi))),
               rep("right", 10))
  
  expect_equal(ehi_factorise_nominal(ehi$ehi_01),
               structure(c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
                         .Label = c("left","ambidexter", "right"), 
                         class = "factor")
  )
  
  expect_equal(as.character(ehi_factorise_nominal(ehi$ehi_01)),
               rep("right", 10)
  )
})


test_that("Check ehi_compute", {
  test <- ehi_compute(ehi, keep_all = FALSE)
  expect_equal(names(test),
               c("ehi_lq", "ehi_nominal", "ehi_lq_cat", "ehi_lqa_cat"))
  
  expect_equal(names(ehi_compute(select(ehi, -ehi_nominal), keep_all = TRUE)),
               c("ehi_le", "ehi_coded", "ehi_date", "ehi_01", 
                 "ehi_02", "ehi_03", "ehi_04", "ehi_05", "ehi_06", "ehi_07", "ehi_08", 
                 "ehi_09", "ehi_10", "ehi_time", "ehi_lq", "ehi_nominal","ehi_lq_cat", "ehi_lqa_cat"))
  
  expect_equal(suppressMessages(ncol(ehi_compute(ehi, keep_all = TRUE))),
               19)
})


test_that("Check ehi_change", {
  
  expect_equal(ehi_change(1:4, direction = 1), 1:4)
  expect_equal(ehi_change(1:4, direction = -1), rep(NA_integer_,4))
  
  expect_equal(ehi_change(-1:-4, direction = -1), 1:4)
  expect_equal(ehi_change(-1:-4, direction = 1), rep(NA_integer_,4))
  
  expect_error(ehi_change(1:4, direction = 0), "should be one of")
})
