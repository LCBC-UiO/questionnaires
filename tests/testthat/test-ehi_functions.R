load(paste0(test_path(), "/data/ehi.rda"))

test_that("Check component calculations", {
  
  expect_equal(ehi_compute_lq(ehi),
               c(73.3333333333333, 11.1111111111111, 85.7142857142857, 100, 
                 100, 100, 100, 100, 100, 89.4736842105263))
  
  expect_equal(ehi_factorise_lq(ehi_compute_lq(ehi)),
               structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
                         .Label = c("Left", 
                                    "Right"), 
                         class = "factor"))
  
  expect_equal(ehi_factorise_lqa(ehi_compute_lq(ehi)),
               structure(c(3L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
                         .Label = c("Left", "Ambidexter", "Right"), class = "factor"))
  
  expect_equal(ehi_factorise_lqa(c(NA, 11, 85, 40, 100, -41, 50, 100, 100, 89), min = -40, max = 40),
               structure(c(NA, 2L, 3L, 2L, 3L, 1L, 3L, 3L, 3L, 3L), 
                         .Label = c("Left", "Ambidexter", "Right"), class = "factor"))
  
  expect_equal(as.character(ehi_factorise_lq(ehi_compute_lq(ehi))),
               rep("Right", 10))
  
  expect_equal(ehi_factorise_nominal(ehi$EHI_01),
               structure(c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
                         .Label = c("Left","Ambidexterous", "Right"), 
                         class = "factor")
  )
  
  expect_equal(as.character(ehi_factorise_nominal(ehi$EHI_01)),
               rep("Right", 10)
  )
})


test_that("Check ehi_compute", {
  
  expect_equal(names(ehi_compute(ehi, keep_all = FALSE)),
               c("EHI_LQ", "EHI_Nominal", "EHI_LQ_cat", "EHI_LQA_cat"))
  
  expect_equal(names(ehi_compute(ehi, keep_all = TRUE)),
               c("EHI_LE", "EHI_Nominal", "EHI_Coded", "EHI_Date", "EHI_01", 
                 "EHI_02", "EHI_03", "EHI_04", "EHI_05", "EHI_06", "EHI_07", "EHI_08", 
                 "EHI_09", "EHI_10", "EHI_Time", "EHI_LQ", "EHI_LQ_cat", "EHI_LQA_cat"))
  
  expect_equal(ncol(ehi_compute(ehi, keep_all = TRUE)),
               18)
})


test_that("Check ehi_change", {
  
  expect_equal(ehi_change(1:4, direction = 1), 1:4)
  expect_equal(ehi_change(1:4, direction = -1), rep(NA_integer_,4))
  
  expect_equal(ehi_change(-1:-4, direction = -1), 1:4)
  expect_equal(ehi_change(-1:-4, direction = 1), rep(NA_integer_,4))
  
  expect_error(ehi_change(1:4, direction = 0), "not valid")
})
