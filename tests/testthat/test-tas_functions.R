context("test-tas_functions")
load("tas.rda")

test_that("Check component calculations", {
  
  expect_equal(tas_reverse(1:6),
               c(5, 4, 3, 2, 1, 0))
  
  expect_equal(tas_compute_reversed(tas, cols = TAS_04, keep_all = FALSE),
               structure(list(TAS_04 = c(2, 5, 4, 4, 5, 1, 3, 5, 4, 3)), 
                         class = "data.frame", 
                         row.names = c(NA, -10L))
  )
  
  expect_equal(unlist(tas_compute_reversed(tas, cols = TAS_04, keep_all = FALSE)),
               c(TAS_041 = 2, TAS_042 = 5, TAS_043 = 4, TAS_044 = 4, TAS_045 = 5, 
                 TAS_046 = 1, TAS_047 = 3, TAS_048 = 5, TAS_049 = 4, TAS_0410 = 3)
  )
  
  expect_equal(ncol(tas_compute_reversed(tas)),
               23)
  
  expect_equal(nrow(tas_compute_reversed(tas)),
               10)
  
})


test_that("Check component calculations", {
  
  
  expect_equal(names(tas_compute(tas, keep_all = FALSE)),
               c("TAS_Fact1_Ident.Feelings", "TAS_Fact2_Descr.Feelings", "TAS_Fact3_Ext.O.Thinking"
               )
  )
  
  expect_equal(names(tas_compute(tas, keep_all = TRUE)),
               c("TAS_01", "TAS_02", "TAS_03", "TAS_04", "TAS_05", "TAS_06", 
                 "TAS_07", "TAS_08", "TAS_09", "TAS_10", "TAS_11", "TAS_12", "TAS_13", 
                 "TAS_14", "TAS_15", "TAS_16", "TAS_17", "TAS_18", "TAS_19", "TAS_20", 
                 "TAS_Fact1_Ident.Feelings", "TAS_Fact2_Descr.Feelings", "TAS_Fact3_Ext.O.Thinking"
               )
  )
  
  expect_equal(ncol(tas_compute(tas, keep_all = TRUE)),
               23
  )
})

