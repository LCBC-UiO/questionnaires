bdi <- read.delim(test_path("data/bdi.tsv"))

test_that("Check component calculations", {
  
  expect_equal(bdi_compute_sum(bdi),
               c(5, 7.5, NA, 11, NA, NA, 3, 4.5, 16.5, NA))
  
  expect_equal(bdi_compute_sum(bdi, max_missing = NULL),
               c(5, 7.5, 2, 11, 2, 2, 3, 4.5, 16.5, 3))
  
  expect_equal(bdi_compute_sum(bdi, max_missing = 1),
               c(5, 7.5, 2, 11, 2, 2, 3, 4.5, 16.5, 3))
  
  expect_equal(bdi_factorise(bdi_compute_sum(bdi)),
               structure(c(1L, 1L, NA, 2L, NA, NA, 1L, 1L, 3L, NA),
                         .Label = c("normal", 
                                    "mild mood disturbance",
                                    "borderline clinical disturbance",
                                    "moderate depression", 
                                    "severe depression",
                                    "extreme depression"), 
                         class = c("ordered", "factor"))
  )
  
  expect_equal(as.character(bdi_factorise(bdi_compute_sum(bdi))),
               c("normal", "normal", NA, "mild mood disturbance", NA, NA, "normal", 
                 "normal", "borderline clinical disturbance", NA)
  )
  
})


test_that("Check full calculations", {
  
  expect_equal(names(bdi_compute(bdi, keep_all=FALSE)),
               c("bdi_sum", "bdi_coded")
  )
  
  expect_equal(names(bdi_compute(bdi, keep_all=FALSE, prefix = NULL)),
               c("sum", "coded")
  )
  
  expect_equal(names(bdi_compute(bdi, keep_all = TRUE, prefix = "bdi2_")),
               c("bdi_coded", "bdi_01", "bdi_02", "bdi_03", "bdi_04", "bdi_05", 
                 "bdi_06", "bdi_07", "bdi_08", "bdi_09", "bdi_10", "bdi_11", "bdi_12", 
                 "bdi_13", "bdi_14", "bdi_15", "bdi_16", "bdi_17", "bdi_18", "bdi_19", 
                 "bdi_19b", "bdi_20", "bdi_21", "bdi2_sum", "bdi2_coded"))
  
  expect_equal(ncol(bdi_compute(bdi, keep_all=FALSE)),
               2)
  
  
  # Check what happens with BDI already exists
  bdi2 <- bdi_compute(bdi, keep_all=TRUE)
  
  bdi2 <- bdi_compute(bdi2, keep_all = FALSE)
  expect_equal(names(bdi2),
               c("bdi_sum", "bdi_coded")
  )
  
  expect_equal(bdi2$bdi_sum,
               c(5, 7.5, NA, 11, NA, NA, 3, 4.5, 16.5, NA))
  
  expect_equal(bdi2$bdi_coded,
               structure(
                 c(1L, 1L, NA, 2L, NA, NA, 1L, 1L, 3L, NA), 
                 .Label = c("normal",
                            "mild mood disturbance",
                            "borderline clinical disturbance",
                            "moderate depression",
                            "severe depression",
                            "extreme depression"), 
                 class = c("ordered", "factor"))
  )
})


test_that("Check BDI restructure of wide", {
  
  dat <- data.frame(
    ID = 1:4, 
    bdi_01_0 = c(NA,1, NA, NA),
    bdi_01_1 = c(1, NA, 1, NA),
    bdi_01_2 = c(NA, NA, 1, NA),
    bdi_01_3 = c(NA, NA, NA, NA),
    bdi_02_0 = c(1, NA, NA, NA),
    bdi_02_1 = c(NA,NA, NA, NA),
    bdi_02_2 = c(NA,1, NA, NA),
    bdi_02_3 = c(NA, NA, NA, 1)
  )
  
  expt <- structure(list(
    ID = 1:4, 
    bdi_01 = c(1, 0, 1.5, NA),
    bdi_02 = c(0, 2, NA, 3)), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -4L))
  
  expect_equal(bdi_restructure(dat),
               expt)
  
  expt <- structure(list(
    ID = 1:4, 
    bdi__01 = c(1, 0, 1.5, NA),
    bdi__02 = c(0, 2, NA, 3)), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -4L))
  
  expect_equal(bdi_restructure(dat, sep="__"),
               expt)
  
  expt <- structure(list(
    ID = 1:3, 
    bdi_02_0 = c(1, NA, NA), 
    bdi_02_1 = c(NA, NA, NA), 
    bdi_02_2 = c(NA, 1, NA), 
    bdi_02_3 = c(NA_real_, NA_real_,NA_real_), 
    bdi_01 = c(1, 0, 1.5)), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -3L))
  
  expect_equal(bdi_restructure(dat, cols = contains("bdi_01")),
               expt)
})
