context("test-ipaq_functions")
load("ipaq.rda")

test_that("Check component calculations", {
  
  tt <- expect_warning(ipaq_time_alter(ipaq),
                       "failed to parse")
  
  expect_equal(tt$IPAQ_2,
               c(0, NA, 60, 60, 0, 210, 60, 25, 0, 45)
  )
  
  expect_equal(tt$IPAQ_4,
               c(30, 90, 60, 60, 60, 120, 0, 15, 180, NA)
  )
  
  expect_equal(tt$IPAQ_6,
               c(60, 20, 60, 25, 90, 20, 0, 75, 60, 30)
  )
  
  
  vig = ipaq_compute_met(tt$IPAQ_6, tt$IPAQ_5b, 8.0)
  mod = ipaq_compute_met(tt$IPAQ_4, tt$IPAQ_3b, 4.0)
  light = ipaq_compute_met(tt$IPAQ_2, tt$IPAQ_1b, 3.3)
  
  
  expect_equal(light,
               c(0, NA, 594, 594, 0, 1386, 792, 247.5, 0, 445.5)
  )
  
  expect_equal(mod,
               c(120, 1080, 240, 720, 240, 1920, 0, 300, 2880, NA)
  )
  
  expect_equal(vig,
               c(3360, 480, 3360, 1400, 2160, 480, 0, 3000, 3360, 960)
  )
  
  
  expect_equal(ipaq_compute_sum(vig, mod, light),
               c(3480, 1560, 4194, 2714, 2400, 3786, 792, 3547.5, 6240, 1405.5)
  )
  
})


test_that("Check component calculations", {
  
  tt <- expect_warning(ipaq_time_alter(ipaq),
                       "failed to parse")
  
  expect_equal(names(ipaq_compute(tt, keep_all = FALSE)),
               c("IPAQ_MET_Vigorous", "IPAQ_MET_Moderate", "IPAQ_MET_Light", 
                 "IPAQ_MET", "IPAQ_Coded"))
  
  expect_equal(names(ipaq_compute(tt)),
               c("IPAQ_1a", "IPAQ_1b", "IPAQ_2", "IPAQ_3a", "IPAQ_3b", "IPAQ_4", 
                 "IPAQ_5a", "IPAQ_5b", "IPAQ_6", "IPAQ_7", "IPAQ_8a", "IPAQ_8b", 
                 "IPAQ_8c", "IPAQ_MET_Vigorous", "IPAQ_MET_Moderate", "IPAQ_MET_Light", 
                 "IPAQ_MET", "IPAQ_Coded"))
  
  expect_equal(ncol(ipaq_compute(tt)),
               18)
})



