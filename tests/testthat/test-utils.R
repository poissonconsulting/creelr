context("utils")

test_that("punctuate_strings", {
  expect_identical(punctuate_strings(c("x")), "x")
  expect_identical(punctuate_strings(c("x","y")), "x or y")
  expect_identical(punctuate_strings(c("x","y","z")), "x, y or z")
  expect_identical(punctuate_strings(c("x","y","z","a")), "x, y, z or a")
  expect_identical(punctuate_strings(c("x","y","z","a"), "and"), "x, y, z and a")
})

test_that("check_by", {
  expect_true(check_by(NULL, "x", "y"))
  expect_true(check_by("x", "x", "y"))
  expect_error(check_by("y", "x", "y"))
  expect_error(check_by("z", "x", "y"))
})
