ipaq <- read.delim(test_path("data/ipaq.tsv"))

test_that("Check ipaq_mets", {
  expect_equal(ipaq_mets(4.0, 3.3, 6.0),
               list(
                 light  = 4.0,
                 moderate = 3.3,
                 vigorous = 6.0
               ))
  
})

test_that("Check component calculations", {
  tt <-
    ipaq_time_alter(ipaq, all_of(c("ipaq_2", "ipaq_4", "ipaq_6", "ipaq_7")))
  
  expect_equal(tt$ipaq_2,
               c(0, NA, 60, 60, 0, 210, 60, 25, 0, 45))
  
  expect_equal(tt$ipaq_4,
               c(30, 90, 60, 60, 60, 120, NA, 15, 180, NA))
  
  expect_equal(tt$ipaq_6,
               c(60, 20, 60, 25, 90, 20, NA, 75, 60, 30))
  
  
  light = ipaq_compute_met(tt$ipaq_6, tt$ipaq_5b, 3.3)
  expect_equal(light,
               c(1386, 198, 1386, 577.5, 891, 198, NA, 1237.5, 1386, 396))
  
  mod = ipaq_compute_met(tt$ipaq_4, tt$ipaq_3b, 4.0)
  expect_equal(mod,
               c(120, 1080, 240, 720, 240, 1920, NA, 300, 2880, NA))
  
  vig = ipaq_compute_met(tt$ipaq_6, tt$ipaq_5b, 8.0)
  expect_equal(vig,
               c(3360, 480, 3360, 1400, 2160, 480, NA, 3000, 3360, 960))
  
  
  expect_equal(
    ipaq_compute_sum(vig, mod, light),
    c(4866, 1758, 4986, 2697.5, 3291, 2598, NA, 4537.5, 7626, 1356)
  )
  
})


test_that("Check component calculations", {
  tt <-
    ipaq_time_alter(ipaq, all_of(c("ipaq_2", "ipaq_4", "ipaq_6", "ipaq_7")))
  
  expect_equal(
    names(ipaq_compute(tt, keep_all = FALSE)),
    c(
      "ipaq_met_vigorous",
      "ipaq_met_moderate",
      "ipaq_met_light",
      "ipaq_met",
      "ipaq_coded"
    )
  )
  
  expect_equal(
    names(ipaq_compute(tt)),
    c(
      "ipaq_1a",
      "ipaq_1b",
      "ipaq_2",
      "ipaq_3a",
      "ipaq_3b",
      "ipaq_4",
      "ipaq_5a",
      "ipaq_5b",
      "ipaq_6",
      "ipaq_7",
      "ipaq_8a",
      "ipaq_8b",
      "ipaq_8c",
      "ipaq_met_vigorous",
      "ipaq_met_moderate",
      "ipaq_met_light",
      "ipaq_met",
      "ipaq_coded"
    )
  )
  
  expect_equal(ncol(ipaq_compute(tt)),
               18)
})
