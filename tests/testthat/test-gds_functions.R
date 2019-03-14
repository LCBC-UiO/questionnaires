context("test-gds_functions")
load("gds.rda")

test_that("Check component calculations", {
  
  expect_equal(gds_binary(c(1,1,0,0,1), 0),
               c(1,1,0,0,1))
  
  expect_equal(gds_binary(c(1,1,0,0,1), 1),
               c(0,0,1,1,0))
  
  expect_equal(gds_binary(c("Yes","Yes","No","No",NA), "Yes"),
               c(0, 0, 1, 1, NA))
  
  tmp <- gds_compute_values(gds, cols = GDS_01, value = 1)
  expect_equal(tmp$GDS_01,
               rep(1,10)
  )
  
  tmp <- gds_compute_values(gds, cols = GDS_05, value = 0)
  expect_equal(tmp$GDS_05,
               c(rep(0,3), 1, rep(0,5),1)
  )
  
  expect_equal(gds_compute_sum(gds),
               c(7, 14, 7, 11, 9, 13, 11, 11, 9, 12)
  )
  
  expect_equal(gds_factorise(gds_compute_sum(gds)),
               structure(c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L), 
                         .Label = c("Normal", 
                                    "Mild depression", 
                                    "Severe depression"), 
                         class = "factor"))
  
  expect_equal(as.character(gds_factorise(gds_compute_sum(gds))),
               c("Normal", "Mild depression", "Normal", "Mild depression", "Normal", 
                 "Mild depression", "Mild depression", "Mild depression", "Normal", 
                 "Mild depression"))
  
})


test_that("Check component calculations", {
  
  
  expect_equal(names(gds_compute(gds, keep_all = FALSE)),
               c("GDS", "GDS_Coded")
  )
  
  expect_equal(names(gds_compute(gds, keep_all = TRUE)),
               c("GDS_01", "GDS_02", "GDS_03", "GDS_04", "GDS_05", "GDS_06", 
                 "GDS_07", "GDS_08", "GDS_09", "GDS_10", "GDS_11", "GDS_12", "GDS_13", 
                 "GDS_14", "GDS_15", "GDS_16", "GDS_17", "GDS_18", "GDS_19", "GDS_20", 
                 "GDS_21", "GDS_22", "GDS_23", "GDS_24", "GDS_25", "GDS_26", "GDS_27", 
                 "GDS_28", "GDS_29", "GDS_30", "GDS", "GDS_Coded")
  )
  
  expect_equal(ncol(gds_compute(gds, keep_all = TRUE)),
               32
  )
})

