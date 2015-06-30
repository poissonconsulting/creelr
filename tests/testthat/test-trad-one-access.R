context("trad-one-access")

test_that("trad_one_access", {
  data(toa_dummy, envir = parent.frame())
  
  toa <- trad_one_access(toa_dummy)
  expect_is(toa, "data.frame")
  expect_equal(nrow(toa), 2)
  expect_equal(ncol(toa), 11)
  expect_equal(as.character(toa$Parameter), c("Effort", "Catch"))
  expect_equal(toa$Estimate, c(752, 476))
})

test_that("day_type", {
  
})
