load(paste0(test_path(), "/data/gds.rda"))

test_that("check binary", {
  
  expect_error(gds_binary(c(1,1,0,NA,1), 0),
               "is not TRUE")
  
  expect_equal(gds_binary(c(1,1,0,NA,1), gds_values(1,0)),
               c(0,0,1,NA,0))
  
  expect_equal(gds_binary(c(1,1,0,0,1), gds_values(0,1)),
               c(1,1,0,0,1))
  
  expect_equal(gds_binary(c("Yes","Yes","No","No",NA,1,0), gds_values("Yes","No")),
               c(0, 0, 1, 1, NA, NA, NA))
})

test_that("check gds_alter_values",{
  
  expect_error(gds_alter_values(gds, cols = GDS_01, value = 1),
               "is not TRUE")
  
  
  tmp <- gds_alter_values(gds, cols = GDS_01, value = gds_values(1, 0))
  expect_equal(tmp$GDS_01,
               rep(1,10)
  )
  
  tmp <- gds_alter_values(gds, cols = GDS_05, value = gds_values(1, 0))
  expect_equal(tmp$GDS_05,
               c(rep(1,3), 0, rep(1,5),0)
  )
  
  tmp <- gds_alter_values(gds, cols = GDS_05, reverse = TRUE, value = gds_values(1, 0))
  expect_equal(tmp$GDS_05,
               c(rep(0,3), 1, rep(0,5),1)
  )
})

test_that("Check sum", {
  
  expect_equal(gds_compute_sum(gds, values = gds_values(yes = 1, no = 0)),
               c(7, 14, 7, 11, 9, 13, 11, 11, 9, 12)
  )
  
  expect_equal(gds_compute_sum(gds, values = gds_values(yes = 0, no = 1)),
               c(23, 16, 23, 19, 21, 17, 19, 19, 21, 18))
  
  expect_error(gds_compute_sum(gds, values = 1),
               "is not TRUE"
  )
  
})


test_that("Check factor",{
  expect_equal(gds_factorise(gds_compute_sum(gds, values = gds_values(yes = 1, no = 0))),
               structure(c(1L, 2L, 1L, 2L, 1L, 2L, 2L, 2L, 1L, 2L), 
                         .Label = c("Normal", 
                                    "Mild depression", 
                                    "Severe depression"), 
                         class = c("ordered", "factor"))
  )
  
  expect_equal(as.character(gds_factorise(gds_compute_sum(gds, values = gds_values(yes = 1, no = 0)))),
               c("Normal", "Mild depression", "Normal", "Mild depression", "Normal", 
                 "Mild depression", "Mild depression", "Mild depression", "Normal", 
                 "Mild depression"))
  
})


test_that("Check compute", {
  
  expect_equal(names(gds_compute(gds, 
                                 values = gds_values("Yes", "No"),
                                 keep_all = FALSE)),
               c("GDS", "GDS_Coded")
  )
  
  expect_equal(names(gds_compute(gds, 
                                 values = gds_values("Yes", "No"),
                                 keep_all = TRUE)),
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

