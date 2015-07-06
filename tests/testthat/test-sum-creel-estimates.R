context("sum_creel_estimates")

test_that("sum_creel_estimates", {
  data(toa_example, envir = parent.frame())
  toa_results <- trad_one_access(toa_example)
  sum_creel <- sum_creel_estimates(toa_results)
  
  expect_is(sum_creel, "data.frame")
  expect_equal(nrow(sum_creel), 2)
  expect_equal(ncol(sum_creel), 6)
  expect_equal(names(sum_creel), c("Year", "Parameter", "Estimate", "SD", "Lower", "Upper"))
  expect_equal(sum_creel$Year, rep(2014, 2))
  expect_equal(as.character(sum_creel$Parameter), c("Effort", "Catch"))
  expect_equal(nrow(sum_creel_estimates(toa_results, by = "Month")), nrow(toa_results))
  expect_error(sum_creel_estimates(toa_example, alpha = 95))
})