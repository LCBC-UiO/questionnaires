load(paste0(test_path(), "/data/ehi.rda"))

test_that("Check component calculations", {
  
  expect_equal(ehi_compute_le(ehi),
               c(20, 0, 16.6666666666667, 31.0344827586207, 9.09090909090909, 
                 13.0434782608696, 20, 0, 20, 31.0344827586207))
  
  expect_equal(ehi_factorise_le(ehi_compute_le(ehi)),
               structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), 
                         .Label = c("Left", 
                                    "Right"), 
                         class = "factor"))
  
  expect_equal(as.character(ehi_factorise_le(ehi_compute_le(ehi))),
               rep("Right", 10))
  
  expect_equal(ehi_factorise_nominal(ehi$EHI_01),
               structure(c(3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L), 
                         .Label = c("Left", 
                                    "Ambidexterious", "Right"), 
                         class = "factor")
  )
  
  expect_equal(as.character(ehi_factorise_nominal(ehi$EHI_01)),
               rep("Right", 10)
  )
})


test_that("Check component calculations", {
  
  
  expect_equal(names(ehi_compute(ehi, keep_all = FALSE)),
               c("EHI_LE", "EHI_Nominal", "EHI_Coded"))
  
  expect_equal(names(ehi_compute(ehi, keep_all = TRUE)),
               c("EHI_LE", "EHI_Nominal", "EHI_Coded", "EHI_Date", "EHI_01", 
                 "EHI_02", "EHI_03", "EHI_04", "EHI_05", "EHI_06", "EHI_07", "EHI_08", 
                 "EHI_09", "EHI_10", "EHI_Time"))
  
  expect_equal(ncol(ehi_compute(ehi, keep_all = TRUE)),
               15)
})


test_that("Check ehi_change", {
  
  expect_equal(ehi_change(1:4, direction = 1), 1:4)
  expect_equal(ehi_change(1:4, direction = -1), rep(1,4))
  
  expect_equal(ehi_change(-1:-4, direction = -1), 1:4)
  expect_equal(ehi_change(-1:-4, direction = 1), rep(1,4))
  
  expect_error(ehi_change(1:4, direction = 0), "not valid")
  
  
})
