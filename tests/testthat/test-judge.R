test_that("strip_extension removes a .R or .r extension", {
  strip_extension <- local_judge()$strip_extension

  expect_equal(strip_extension("01-basics.R"), "01-basics")
  expect_equal(strip_extension("01-basics.r"), "01-basics")
})

test_that("strip_extension removes only the last extension", {
  strip_extension <- local_judge()$strip_extension

  expect_equal(strip_extension("basics.R.R"), "basics.R")
})

test_that("strip_extension keeps other extensions", {
  strip_extension <- local_judge()$strip_extension

  expect_equal(strip_extension("basics.Rmd"), "basics.Rmd")
  expect_equal(strip_extension("basics"), "basics")
})

test_that("strip_leading_digit removes a numeric prefix and its dash", {
  strip_leading_digit <- local_judge()$strip_leading_digit

  expect_equal(strip_leading_digit("01-basics"), "basics")
})

test_that("strip_leading_digit removes only the first prefix", {
  strip_leading_digit <- local_judge()$strip_leading_digit

  expect_equal(strip_leading_digit("1-2-basics"), "2-basics")
})

test_that("strip_leading_digit keeps names without a numeric prefix", {
  strip_leading_digit <- local_judge()$strip_leading_digit

  expect_equal(strip_leading_digit("my-basics"), "my-basics")
  expect_equal(strip_leading_digit("01basics"), "01basics")
})
