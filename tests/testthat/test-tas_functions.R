tas <- read.delim(test_path("data/tas.tsv"))

test_that("Check component calculations", {
  
  expect_equal(tas_reverse(1:6),
               c(5, 4, 3, 2, 1, 0))
  
  expect_equal(tas_compute_reversed(tas, cols = tas_04, keep_all = FALSE),
               structure(list(tas_04 = c(2, 5, 4, 4, 5, 1, 3, 5, 4, 3)), 
                         class = "data.frame", 
                         row.names = c(NA, -10L))
  )
  
  expect_equal(unlist(tas_compute_reversed(tas, cols = tas_04, keep_all = FALSE)),
               c(tas_041 = 2, tas_042 = 5, tas_043 = 4, tas_044 = 4, tas_045 = 5, 
                 tas_046 = 1, tas_047 = 3, tas_048 = 5, tas_049 = 4, tas_0410 = 3)
  )
  
  expect_equal(ncol(tas_compute_reversed(tas)),
               23)
  
  expect_equal(nrow(tas_compute_reversed(tas)),
               10)
  
})


test_that("Check component calculations", {
  
  
  expect_equal(names(tas_compute(tas, keep_all = FALSE)),
               c("tas_fact1_identify", "tas_fact2_describe", "tas_fact3_thinking"
               )
  )
  
  expect_equal(names(tas_compute(tas, keep_all = FALSE, prefix = NULL)),
               c("fact1_identify", "fact2_describe", "fact3_thinking"
               )
  )
  
  
  expect_equal(names(tas_compute(tas, keep_all = TRUE)),
               c("tas_fact1_ident.feelings", "tas_fact2_descr.feelings", "tas_fact3_ext.o.thinking",
                 "tas_01", "tas_02", "tas_03", "tas_04", "tas_05", "tas_06", 
                 "tas_07", "tas_08", "tas_09", "tas_10", "tas_11", "tas_12", "tas_13", 
                 "tas_14", "tas_15", "tas_16", "tas_17", "tas_18", "tas_19", "tas_20", 
                 "tas_fact1_identify", "tas_fact2_describe", "tas_fact3_thinking"
               )
  )
  
  expect_equal(ncol(tas_compute(tas, keep_all = TRUE)),
               26
  )
})

