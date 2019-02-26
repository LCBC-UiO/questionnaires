context("psqi-compute_components")
library(tidyverse)
load(paste0(here::here(), "/tests/testthat/data/psqi.rda"))

test_that("Check component calculations", {
  
  expect_equal(compute_comp2(minBeforeSleep = test$PSQI_02, 
                             noSleep30min = test$PSQI_05a),
               c(0, 0, 1, 2, 2, 1, 0, 1, 1, 0, 0, 2))
  
  expect_equal(compute_comp3(hoursSleep = test$PSQI_04),
               c(1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1))
  
  expect_equal(compute_comp4(hoursSleep = test$PSQI_04, 
                bedtime = test$PSQI_01, 
                risingtime = test$PSQI_03),
               c(NA, NA, NA, NA, 1, NA, NA, NA, 1, NA, 3, 2))
  
  expect_equal(compute_comp5(data = test, 
                noSleep30min = PSQI_05a, 
                sleepTroubles = matches("^PSQI_05[a-j]$")),
               c(0, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1))
  
  expect_equal(compute_comp7(keepAwake = test$PSQI_08, 
                keepEnthused = test$PSQI_09),
               c(0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0))
  
  
})




context("psqi-compute_psqi")

test_that("Check component calculations", {
  
  
  expect_equal(names(compute_psqi(test, keep_all = FALSE)),
               c("PSQI_Comp1_SleepQuality", "PSQI_Comp2_Latency", "PSQI_Comp3_Duration",
                 "PSQI_Comp4_Efficiency", "PSQI_Comp5_Problems", "PSQI_Comp6_Medication",
                 "PSQI_Comp7_Tired", "PSQI_Global"))
  
  expect_equal(names(compute_psqi(test, components = 1:3, keep_all = FALSE)),
               c("PSQI_Comp1_SleepQuality", "PSQI_Comp2_Latency", "PSQI_Comp3_Duration"))
  
  expect_equal(ncol(compute_psqi(test, components = 1:7, keep_all = TRUE)),
               39)
})


