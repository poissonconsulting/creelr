context("day_types")

test_that("day_type", {
  data(toa_dummy, envir = parent.frame())
  expect_error(day_type(toa_dummy))
  dt <- day_type(toa_dummy$Date)
  expect_is(dt, "ordered")
  expect_equal(as.character(dt), toa_dummy$DayType)
  data(toa_example, envir = parent.frame())
  dt2 <- day_type(toa_example$Date, weekend = c("Friday", "Saturday", "Sunday"))
  expect_equal(dt2, as.ordered(toa_example$DayType))
})

test_that("nday_type_month",{
  ndays <- nday_type_month(12)
  expect_is(ndays, "integer")
  expect_equal(names(ndays), c("Week", "Weekend"))
  dec <- c(21, 10)
  names(dec) <- c("Week", "Weekend")
  expect_equal(ndays, dec)
})

test_that("sub_holiday", {
  data(toa_example, envir = parent.frame())
  holi <- as.Date("2014-05-29")
  expect_error(sub_holiday(toa_example, holidays = "x"))
  h1 <- sub_holiday(toa_example)
  h2 <- sub_holiday(toa_example, holidays = holi)
  expect_true(sum(h1 == "Weekend") < sum(h2 == "Weekend"))
})