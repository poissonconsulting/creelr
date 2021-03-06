context("day_types")

test_that("day_type", {
  data(toa_dummy, envir = parent.frame())
  expect_error(day_type(toa_dummy))
  dt <- day_type(toa_dummy$Date)
  expect_is(dt, "factor")
  expect_equal(as.character(dt), c("Week", "Week", "Weekend", "Weekend", "Week",
                                   "Week", "Weekend", "Weekend", "Week", "Week",
                                   "Weekend", "Weekend", "Week", "Week", "Weekend",
                                   "Weekend"))
  expect_error(day_type(toa_dummy, holidays = "x"))
  
  h1 <- day_type(toa_dummy$Date)
  h2 <- day_type(toa_dummy$Date, holidays = as.Date("2010-02-02"))
  expect_true(sum(h1 == "Weekend") < sum(h2 == "Weekend"))
})

test_that("nday_type_month",{
  ndays <- nday_type_month(12, 2000, c("Saturday", "Sunday"), NULL)
  expect_is(ndays, "data.frame")
  expect_equal(colnames(ndays), c("DayType", "TotalDays"))
  expect_equal(ndays$DayType, factor(c("Week", "Weekend"), levels = c("Week", "Weekend")))
  expect_equal(ndays$TotalDays, c(21, 10))
})
