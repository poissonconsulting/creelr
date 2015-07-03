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
  expect_equal(toa$Coverage_WKND, toa$Coverage_WK)
  expect_equal(trad_one_access(toa_dummy, am = 0.6, weighted = TRUE)$Coverage_WKND, rep(3.8, 2))
  expect_error(trad_one_access(toa_dummy, am = 2))
  expect_error(trad_one_access(toa_dummy, am = -2))
})

test_that("check_period", {
  data(toa_dummy, envir = parent.frame())
  
  bad <- toa_dummy
  bad$Date[2] <- bad$Date[1]
  expect_error(trad_one_access(bad))
})

test_that("check_weighted", {
  data(toa_dummy, envir = parent.frame())
  
  expect_equal(trad_one_access(toa_dummy)$Coverage_WK, c(8,8))
  expect_equal(trad_one_access(toa_dummy)$Coverage_WKND, c(8,8))
  expect_equal(trad_one_access(toa_dummy, weighted = TRUE)$Coverage_WK, c(4,4))
  expect_equal(trad_one_access(toa_dummy, weighted = TRUE)$Coverage_WKND, c(4,4))
  expect_equal(trad_one_access(toa_dummy, am = 0.6, weighted = TRUE)$Coverage_WK, c(4, 4))
  expect_equal(trad_one_access(toa_dummy, am = 0.6, weighted = TRUE)$Coverage_WKND, c(3.8, 3.8))
})
