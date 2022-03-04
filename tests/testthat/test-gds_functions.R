gds <- read.delim(test_path("data/gds.tsv"))

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
  
  expect_error(gds_alter_values(gds, cols = gds_01, value = 1),
               "is not TRUE")
  
  
  tmp <- gds_alter_values(gds, cols = gds_01, value = gds_values(1, 0))
  expect_equal(tmp$gds_01,
               rep(1,10)
  )
  
  tmp <- gds_alter_values(gds, cols = gds_05, value = gds_values(1, 0))
  expect_equal(tmp$gds_05,
               c(rep(1,3), 0, rep(1,5),0)
  )
  
  tmp <- gds_alter_values(gds, cols = gds_05, reverse = TRUE, value = gds_values(1, 0))
  expect_equal(tmp$gds_05,
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
                         .Label = c("normal", 
                                    "mild depression", 
                                    "severe depression"), 
                         class = c("ordered", "factor"))
  )
  
  expect_equal(as.character(gds_factorise(gds_compute_sum(gds, values = gds_values(yes = 1, no = 0)))),
               c("normal", "mild depression", "normal", "mild depression", "normal", 
                 "mild depression", "mild depression", "mild depression", "normal", 
                 "mild depression"))
  
})


test_that("Check compute", {
  
  expect_equal(names(gds_compute(gds, 
                                 values = gds_values("Yes", "No"),
                                 keep_all = FALSE)),
               c("gds_sum", "gds_coded")
  )
  
  expect_equal(names(gds_compute(gds, 
                                 values = gds_values("Yes", "No"),
                                 prefix = "gds2_",
                                 keep_all = TRUE)),
               c("gds", "gds_coded","gds_01", "gds_02", "gds_03", "gds_04", "gds_05", "gds_06", 
                 "gds_07", "gds_08", "gds_09", "gds_10", "gds_11", "gds_12", "gds_13", 
                 "gds_14", "gds_15", "gds_16", "gds_17", "gds_18", "gds_19", "gds_20", 
                 "gds_21", "gds_22", "gds_23", "gds_24", "gds_25", "gds_26", "gds_27", 
                 "gds_28", "gds_29", "gds_30", "gds2_sum", "gds2_coded")
  )
  
  expect_equal(ncol(gds_compute(gds, keep_all = TRUE)),
               34
  )
})

