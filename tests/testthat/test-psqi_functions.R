psqi <- read.delim(test_path("data/psqi.tsv"))

test_that("Check component calculations", {
  
  tmp <- data.frame(comp2 = psqi_compute_comp2(psqi$psqi_02, psqi$psqi_05a))
  expect_equal(tmp$comp2,
               c(0, 0, 1, 2, 2, 1, 0, 1, 1, 0, 0, 2))
  
  tmp$comp3 <- psqi_compute_comp3(hours_sleep = psqi$psqi_04)
  expect_equal(tmp$comp3,
               c(1, 0, 0, 0, 1, 1, 0, 1, 1, 0, 1, 1))
  
  tmp$comp4 <- psqi_compute_comp4(hours_sleep = psqi$psqi_04, 
                                  bedtime = psqi$psqi_01, 
                                  risingtime = psqi$psqi_03,
                                  risingtime_func = lubridate::hms,
                                  bedtime_func = lubridate::hms)
  expect_equal(tmp$comp4,
               c(0, 0, 0, NA, 1, 0, 0, 0, 1, 0, 3, 2))
  
  tmp$comp5 <- psqi_compute_comp5(data = psqi, 
                                  sleep_troubles = matches("^psqi_05[b-j]$"))
  expect_equal(tmp$comp5,
               c(0, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1))
  
  tmp$comp7 <- psqi_compute_comp7(keep_awake = psqi$psqi_08, 
                                  keep_enthused = psqi$psqi_09)
  expect_equal(tmp$comp7,
               c(0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0))
  
  tmp$comp1 <- psqi$psqi_06
  tmp$comp6 <- psqi$psqi_07
  tmp$global <- psqi_compute_global(tmp, cols = matches("comp[1-7]+"))  
  
  expect_equal(tmp$global,
               c(1, 2, 5, NA, 7, 4, 1, 10, 6, 4, 7, 6)
  )
})


test_that("Check component calculations", {
  
  
  expect_equal(names(psqi_compute(psqi, keep_all = FALSE,
                                  risingtime_func = lubridate::hms,
                                  bedtime_func = lubridate::hms)),
               c("psqi_comp1_quality", "psqi_comp2_latency", "psqi_comp3_duration",
                 "psqi_comp4_efficiency", "psqi_comp5_problems", "psqi_comp6_medication",
                 "psqi_comp7_tired", "psqi_global"))
  
  expect_equal(names(psqi_compute(psqi, components = 1:3, keep_all = FALSE,
                                  risingtime_func = lubridate::hms,
                                  bedtime_func = lubridate::hms)),
               c("psqi_comp1_quality", "psqi_comp2_latency", "psqi_comp3_duration"))
  
  expect_equal(ncol(psqi_compute(psqi, components = 1:7, keep_all = TRUE,
                                 risingtime_func = lubridate::hms,
                                 bedtime_func = lubridate::hms)),
               46)
  
  expect_error(psqi_compute_global(psqi, max_missing = 8),
               "max_missing must be between 0 and 6")
})


