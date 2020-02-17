# test_that("check that time_hms2deciH works", {
#   time_hms2deciH(c("00:20:12", "22:30:10", "08:21:58", "01:21:20", "01:40:30"))
# })

test_that("check that time_deci2period works", {
  expect_equal(time_deci2period(c(4.3, 2.7, 1, .04)),
                                c("04:18", "02:42", "01:00", "00:02")
               )

  expect_equal(time_deci2period(c(4.543, 2.722, 1, .0404), type = "hms"),
               c("04:32:35", "02:43:19", "01:00:00", "00:02:25")
               )
  
  expect_equal(time_deci2period(c(4.543, 2.722, 1, .0404), unit = "minute", type = "hms"),
               c("00:04:33", "00:02:43", "00:01:00", "00:00:02")
               )
  
  expect_equal(time_deci2period(c(4.543, 2.722, 1, .0404), unit = "second", type = "hms"),
               c("00:00:05", "00:00:03", "00:00:01", "00:00:00")
               )
})

test_that("check that time_alter works", {
  expect_equal(time_alter(c("00:20", "22:30", "08:21", "01:21", "01:40")),
               c(20, 1350, 501, 81, 100))
  
  expect_equal(time_alter(c("00:20", "22:30", "08:21", "01:21", "01:40"), "hour"),
               c(0.333333333333333, 22.5, 8.35, 1.35, 1.66666666666667))
  
  expect_equal(time_alter(c("00:20", "22:30", "08:21", "01:21", "01:40"), "second"),
               c(1200, 81000, 30060, 4860, 6000))
  
})

test_that("check that is.hms works", {
  
  t <- data.frame(
    hm = c("00:20", "22:30", "08:21", "01:21", "01:40"),
    hms = c("00:20:00", "22:30:12", "08:21:14", "01:21:49", "01:40:37"),
    strinsAsFactor = FALSE
  )
  expect_false(is.hm(t$hm))
  expect_false(is.hms(t$hms))
  
  expect_true(is.hm(lubridate::hm(t$hm)))
  expect_true(is.hms(lubridate::hms(t$hms)))
  
})

