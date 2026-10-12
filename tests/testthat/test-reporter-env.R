test_that("get_reporter returns a DodonaReporter", {
  judge <- local_judge()

  expect_s3_class(judge$get_reporter(), "DodonaReporter")
})

test_that("get_reporter returns the same reporter every time", {
  judge <- local_judge()

  expect_identical(judge$get_reporter(), judge$get_reporter())
})
