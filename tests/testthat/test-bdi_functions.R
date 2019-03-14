context("test-bdi_functions")
load("bdi.rda")

test_that("Check component calculations", {
  
  expect_equal(bdi_compute_sum(bdi),
               c(5, 7.5, 2, 11, 2, 2, 3, 4.5, 16.5, 3))
  
  expect_equal(bdi_factorise(bdi_compute_sum(bdi)),
               structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L), 
                         .Label = c("Minimal depression", 
                                    "Mild depression", "Moderate depression", "Severe depression"), 
                         class = "factor")
  )
  
  expect_equal(as.character(bdi_factorise(bdi_compute_sum(bdi))),
               c("Minimal depression", "Minimal depression", "Minimal depression", 
                 "Minimal depression", "Minimal depression", "Minimal depression", 
                 "Minimal depression", "Minimal depression", "Mild depression", 
                 "Minimal depression")
  )
  
})


test_that("Check component calculations", {
  
  expect_equal(names(bdi_compute(bdi, keep_all=FALSE)),
               c("BDI", "BDI_Coded")
  )
  
  expect_equal(names(bdi_compute(bdi, keep_all = TRUE)),
               c("BDI_Coded", "BDI_01", "BDI_02", "BDI_03", "BDI_04", "BDI_05", 
                 "BDI_06", "BDI_07", "BDI_08", "BDI_09", "BDI_10", "BDI_11", "BDI_12", 
                 "BDI_13", "BDI_14", "BDI_15", "BDI_16", "BDI_17", "BDI_18", "BDI_19", 
                 "BDI_19b", "BDI_20", "BDI_21", "BDI"))
  
  expect_equal(ncol(bdi_compute(bdi, keep_all=FALSE)),
               2)
})



