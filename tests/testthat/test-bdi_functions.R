load(paste0(test_path(),"/data/bdi.rda"))

test_that("Check component calculations", {
  
  expect_equal(bdi_compute_sum(bdi),
               c(5, 7.5, NA, 11, NA, NA, 3, 4.5, 16.5, NA))
  
  expect_equal(bdi_compute_sum(bdi, max_missing = NULL),
               c(5, 7.5, 2, 11, 2, 2, 3, 4.5, 16.5, 3))
  
  expect_equal(bdi_compute_sum(bdi, max_missing = 1),
               c(5, 7.5, 2, 11, 2, 2, 3, 4.5, 16.5, 3))
  
  expect_equal(bdi_factorise(bdi_compute_sum(bdi)),
               structure(c(1L, 1L, NA, 1L, NA, NA, 1L, 1L, 2L, NA), 
                         .Label = c("Minimal depression", "Mild depression", 
                                    "Moderate depression", "Severe depression"
                         ), class = "factor")
  )
  
  expect_equal(as.character(bdi_factorise(bdi_compute_sum(bdi))),
               c("Minimal depression", "Minimal depression", NA, "Minimal depression", 
                 NA, NA, "Minimal depression", "Minimal depression", "Mild depression", 
                 NA)
  )
  
})


test_that("Check full calculations", {
  
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
  
  
  # Check what happens with BDI already exists
  bdi2 <- bdi_compute(bdi, keep_all=TRUE)
  
  bdi2 <- bdi_compute(bdi2, keep_all = FALSE)
  expect_equal(names(bdi2),
               c("BDI", "BDI_Coded")
  )
  
  expect_equal(bdi2$BDI,
               c(5, 7.5, NA, 11, NA, NA, 3, 4.5, 16.5, NA))
  
  expect_equal(bdi2$BDI_Coded,
               structure(
                 c(1L, 1L, NA, 1L, NA, NA, 1L, 1L, 2L, NA), 
                 .Label = c("Minimal depression", "Mild depression", "Moderate depression", "Severe depression"
                 ), class = "factor"))
  
  
  # Check predicate
  bdi2 <- bdi_compute(bdi, predicate = BDI_19b == 1)
  
  expect_equal(bdi2$BDI,
               c(NA, NA, NA, 11, NA, NA, NA, NA, 16.5, NA))
  
  expect_equal(bdi2$BDI_Coded,
               structure(c(NA, NA, NA, 1L, NA, NA, NA, NA, 2L, NA), 
                         .Label = c("Minimal depression", "Mild depression", "Moderate depression", "Severe depression"
               ), class = "factor"))
})


test_that("Check BDI restructure of wide", {
  
  dat <- data.frame(
    ID = 1:4, 
    BDI_01_0 = c(NA,1, NA, NA),
    BDI_01_1 = c(1, NA, 1, NA),
    BDI_01_2 = c(NA, NA, 1, NA),
    BDI_01_3 = c(NA, NA, NA, NA),
    BDI_02_0 = c(1, NA, NA, NA),
    BDI_02_1 = c(NA,NA, NA, NA),
    BDI_02_2 = c(NA,1, NA, NA),
    BDI_02_3 = c(NA, NA, NA, 1)
  )
  
  expt <- structure(list(
    ID = 1:4, 
    BDI_01 = c(1, 0, 1.5, NA),
    BDI_02 = c(0, 2, NA, 3)), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -4L))
  
  expect_equal(bdi_restructure(dat),
               expt)
  
  expt <- structure(list(
    ID = 1:4, 
    BDI__01 = c(1, 0, 1.5, NA),
    BDI__02 = c(0, 2, NA, 3)), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -4L))
  
  expect_equal(bdi_restructure(dat, sep="__"),
               expt)
  
  expt <- structure(list(
    ID = 1:3, 
    BDI_02_0 = c(1, NA, NA), 
    BDI_02_1 = c(NA, NA, NA), 
    BDI_02_2 = c(NA, 1, NA), 
    BDI_02_3 = c(NA_real_, NA_real_,NA_real_), 
    BDI_01 = c(1, 0, 1.5)), 
    class = c("tbl_df", "tbl", "data.frame"), 
    row.names = c(NA, -3L))
  
  expect_equal(bdi_restructure(dat, cols = contains("BDI_01")),
               expt)
})
