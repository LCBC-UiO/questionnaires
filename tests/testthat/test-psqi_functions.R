context("test-psqi_functions")
load("psqi.rda")

test_that("Check component calculations", {
  
  expect_equal(psqi_compute_comp2(minBeforeSleep = psqi$PSQI_02, 
                             noSleep30min = psqi$PSQI_05a),
               c(0, 0, 1, 2, 2, 1, 0, 1, 1, 0, 0, 2))
  
  expect_equal(psqi_compute_comp3(hoursSleep = psqi$PSQI_04),
               c(1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1))
  

  expect_equal(psqi_compute_comp4(hoursSleep = psqi$PSQI_04, 
                bedtime = psqi$PSQI_01, 
                risingtime = psqi$PSQI_03),
               c(0, 0, 0, NA, 1, 0, 0, 0, 1, 0, 3, 2))

  expect_equal(psqi_compute_comp5(data = psqi, 
                noSleep30min = PSQI_05a, 
                sleepTroubles = matches("^PSQI_05[a-j]$")),
               c(0, 1, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1))
  
  expect_equal(psqi_compute_comp7(keepAwake = psqi$PSQI_08, 
                keepEnthused = psqi$PSQI_09),
               c(0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0))
  
  
})


test_that("Check component calculations", {
  
  
  expect_equal(names(psqi_compute(psqi, keep_all = FALSE)),
               c("PSQI_Comp1_SleepQuality", "PSQI_Comp2_Latency", "PSQI_Comp3_Duration",
                 "PSQI_Comp4_Efficiency", "PSQI_Comp5_Problems", "PSQI_Comp6_Medication",
                 "PSQI_Comp7_Tired", "PSQI_Global"))
  
  expect_equal(names(psqi_compute(psqi, components = 1:3, keep_all = FALSE)),
               c("PSQI_Comp1_SleepQuality", "PSQI_Comp2_Latency", "PSQI_Comp3_Duration"))
  
  expect_equal(ncol(psqi_compute(psqi, components = 1:7, keep_all = TRUE)),
               39)
})


