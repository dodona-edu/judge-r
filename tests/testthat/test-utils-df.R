test_that("round_df rounds numeric columns to six significant digits", {
  round_df <- local_judge()$round_df

  expect_equal(round_df(data.frame(x = c(1.23456789, 123456.789))), data.frame(x = c(1.23457, 123457)))
})

test_that("round_df leaves other columns alone", {
  round_df <- local_judge()$round_df
  df <- data.frame(name = c("a", "b"), level = factor(c("low", "high")), ok = c(TRUE, FALSE))

  expect_identical(round_df(df), df)
})

test_that("round_df keeps a tibble a tibble", {
  round_df <- local_judge()$round_df

  expect_s3_class(round_df(tibble::tibble(x = 1 / 3)), "tbl_df")
})

test_that("compare_data_frames_row_independent accepts the same rows in another order", {
  compare <- local_judge()$compare_data_frames_row_independent
  expected <- data.frame(id = c(1, 2, 3), name = c("a", "b", "c"))
  generated <- data.frame(id = c(3, 1, 2), name = c("c", "a", "b"))

  expect_true(compare(expected, generated))
})

test_that("compare_data_frames_row_independent rejects a different value", {
  compare <- local_judge()$compare_data_frames_row_independent
  expected <- data.frame(id = c(1, 2), name = c("a", "b"))
  generated <- data.frame(id = c(1, 2), name = c("a", "x"))

  expect_false(compare(expected, generated))
})

test_that("compare_data_frames_row_independent rejects a missing or an extra row", {
  compare <- local_judge()$compare_data_frames_row_independent
  two_rows <- data.frame(id = c(1, 2), name = c("a", "b"))
  three_rows <- data.frame(id = c(1, 2, 3), name = c("a", "b", "c"))

  expect_false(compare(three_rows, two_rows))
  expect_false(compare(two_rows, three_rows))
})

test_that("compare_data_frames_row_independent counts duplicate rows", {
  compare <- local_judge()$compare_data_frames_row_independent
  expected <- data.frame(id = c(1, 1, 2), name = c("a", "a", "b"))
  generated <- data.frame(id = c(1, 2, 2), name = c("a", "b", "b"))

  expect_false(compare(expected, generated))
})

test_that("compare_data_frames_row_independent depends on the column order", {
  compare <- local_judge()$compare_data_frames_row_independent

  expect_false(compare(data.frame(id = 1, name = "a"), data.frame(name = "a", id = 1)))
})

test_that("compare_data_frames_row_independent rejects other column names", {
  compare <- local_judge()$compare_data_frames_row_independent

  expect_false(compare(data.frame(id = 1, name = "a"), data.frame(key = 1, label = "a")))
})

test_that("compare_data_frames_row_independent accepts two empty data frames", {
  compare <- local_judge()$compare_data_frames_row_independent

  expect_true(compare(data.frame(x = numeric()), data.frame(x = numeric())))
})

test_that("compare_data_frames_row_independent passes extra arguments to all.equal", {
  compare <- local_judge()$compare_data_frames_row_independent
  expected <- data.frame(id = c(1, 2), value = c(1, 2))
  generated <- data.frame(id = c(1, 2), value = c(1.001, 2))

  expect_false(compare(expected, generated))
  expect_true(compare(expected, generated, tolerance = 0.01))
})

test_that("compare_data_frames_row_independent matches near-equal rows that sort differently", {
  compare <- local_judge()$compare_data_frames_row_independent
  expected <- data.frame(x = c(1, 1 + 1e-12, 2), name = c("q", "p", "r"))
  generated <- data.frame(x = c(1 + 1e-12, 1, 2), name = c("q", "p", "r"))

  expect_true(compare(expected, generated))
})

test_that("compare_data_frames_row_independent still rejects a near-equal row with another value", {
  compare <- local_judge()$compare_data_frames_row_independent
  expected <- data.frame(x = c(1, 1 + 1e-12, 2), name = c("q", "p", "r"))
  generated <- data.frame(x = c(1 + 1e-12, 1, 2), name = c("q", "p", "s"))

  expect_false(compare(expected, generated))
})

test_that("compare_data_frames_row_independent rejects a single column with another name", {
  compare <- local_judge()$compare_data_frames_row_independent

  # Bug #106: testDF ignores the column name when it compares one-column data frames row by row
  expect_failure(expect_false(compare(data.frame(id = c(1, 2)), data.frame(key = c(1, 2)))))
})

test_that("dataframe_all_equal accepts equal data frames", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  df <- data.frame(id = 1:3, name = c("a", "b", "c"))

  result <- dataframe_all_equal(df, df)

  expect_true(result$equal)
  expect_equal(result$feedback, "")
})

test_that("dataframe_all_equal compares after rounding to six significant digits", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal

  expect_true(dataframe_all_equal(data.frame(x = 1.0000001), data.frame(x = 1))$equal)
})

test_that("dataframe_all_equal treats integer and double columns alike", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal

  expect_true(dataframe_all_equal(data.frame(x = 1:2), data.frame(x = c(1, 2)))$equal)
})

test_that("dataframe_all_equal reports the first row that differs", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:10, name = letters[1:10])
  generated <- expected
  generated$name[c(5, 7)] <- "x"

  result <- dataframe_all_equal(generated, expected)

  expect_false(result$equal)
  expect_equal(result$feedback, "unexpected row with rownumber: 5")
})

test_that("dataframe_all_equal shows the rows around the first row that differs", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:10, name = letters[1:10])
  generated <- expected
  generated$name[5] <- "x"

  result <- dataframe_all_equal(generated, expected, before_error = 2, after_error = 2)

  expect_equal(result$df1_rows, 3:7)
  expect_equal(result$df2_rows, 3:7)
})

test_that("dataframe_all_equal shows the first row when it differs", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:10, name = letters[1:10])
  generated <- expected
  generated$name[1] <- "x"

  result <- dataframe_all_equal(generated, expected)

  expect_equal(result$feedback, "unexpected row with rownumber: 1")
  # Bug #108: testDF__inReview__ leaves out the wrong row when it is one of the first rows
  expect_failure(expect_contains(result$df1_rows, 1))
})

test_that("dataframe_all_equal reports a missing or an extra row at the end", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:3, name = c("a", "b", "c"))

  expect_equal(dataframe_all_equal(expected[1:2, ], expected)$feedback, "unexpected row with rownumber: 3")
  expect_equal(dataframe_all_equal(expected, expected[1:2, ])$feedback, "unexpected row with rownumber: 3")
})

test_that("dataframe_all_equal puts the columns in the order of the first data frame", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  generated <- data.frame(name = c("a", "b"), id = 1:2)
  expected <- data.frame(id = 1:2, name = c("a", "b"))

  result <- dataframe_all_equal(generated, expected)

  expect_true(result$equal)
  expect_equal(result$df2_cols, c("name", "id"))
})

test_that("dataframe_all_equal compares the column order when asked to", {
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  generated <- data.frame(name = c("a", "b"), id = 1:2)
  expected <- data.frame(id = 1:2, name = c("a", "b"))

  result <- dataframe_all_equal(generated, expected, ignore_col_order = FALSE)

  expect_false(result$equal)
  expect_equal(result$feedback, "unexpected row with rownumber: 1")
})

test_that("dataframe_all_equal accepts the same rows in another order when the row order is ignored", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:3, name = c("a", "b", "c"))

  result <- dataframe_all_equal(expected[c(3, 1, 2), ], expected, ignore_row_order = TRUE)

  expect_true(result$equal)
  expect_equal(result$df2_rows, c(3L, 1L, 2L))
})

test_that("dataframe_all_equal reports a row that is not in the expected data frame", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:3, name = c("a", "b", "c"))
  generated <- data.frame(id = 1:3, name = c("a", "x", "c"))

  result <- dataframe_all_equal(generated, expected, ignore_row_order = TRUE)

  expect_false(result$equal)
  expect_equal(result$feedback, "Your dataframe contains at least one row which is not found in the solution.")
})

test_that("dataframe_all_equal reports a missing row when the row order is ignored", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:3, name = c("a", "b", "c"))

  result <- dataframe_all_equal(expected[1:2, ], expected, ignore_row_order = TRUE)

  expect_false(result$equal)
  expect_equal(result$feedback, "Your dataframe is missing at least one row.")
})

test_that("dataframe_all_equal reports an extra row when the row order is ignored", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  expected <- data.frame(id = 1:3, name = c("a", "b", "c"))

  result <- dataframe_all_equal(expected, expected[1:2, ], ignore_row_order = TRUE)

  expect_false(result$equal)
  expect_equal(result$feedback, "Your dataframe contains at least one row too much.")
})

test_that("dataframe_all_equal does not use deprecated dplyr functions", {
  # Lifecycle warns once per session; a judge run is a fresh session, a test run is not.
  withr::local_options(lifecycle_verbosity = "warning")
  dataframe_all_equal <- local_judge()$dataframe_all_equal
  # One row, so all_equal() runs once and expect_no_warning() sees its only warning.
  expected <- data.frame(id = 1, name = "a")

  # Bug #109: testDF__inReview__ shows a dplyr::all_equal() deprecation warning
  expect_failure(expect_no_warning(
    dataframe_all_equal(expected, expected, ignore_row_order = TRUE),
    class = "lifecycle_warning_deprecated"
  ))
})
