context("trad-one-access")

test_that("trad_one_access", {
  data(toa_dummy, envir = parent.frame())
  
  toa <- trad_one_access(toa_dummy)
  expect_is(toa, "data.frame")
  expect_equal(nrow(toa), 2)
  expect_equal(ncol(toa), 11)
  expect_equal(names(toa), c("Year", "Month", "Parameter", "Estimate", "SD",
                             "Lower", "Upper", "WK", "WKND", "Coverage_WK",
                             "Coverage_WKND"))
  expect_equal(as.character(toa$Parameter), c("Effort", "Catch"))
  expect_equal(toa$Estimate, c(752, 476))
  expect_equal(toa$SD > 0, c(TRUE, TRUE))
  expect_equal(toa$Lower < toa$Upper, c(TRUE, TRUE))
  expect_equal(toa$WK, rep(20, 2))
  expect_equal(toa$WKND, rep(8, 2))
  expect_error(trad_one_access(toa_dummy, am = 2))
  expect_error(trad_one_access(toa_dummy, am = -2))
})

test_that("check_period", {
  data(toa_example, envir = parent.frame())
  
  expect_true(check_period(toa_example))
  bad <- toa_example
  bad$Period[2] <- "PM"
  expect_false(check_period(bad))
  expect_error(trad_one_access(bad))
})

