test_that("testDF accepts the same data frame", {
  judge <- local_judge_with(df = data.frame(id = 1:3, name = c("a", "b", "c")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    \n  3  c    ", description = "df"),
    list(command = "append-message", message = list(description = "Only the first five rows of the dataframe are shown.", format = "plain")),
    list(command = "close-test", generated = " id  name \n---  -----\n  1  a    \n  2  b    \n  3  c    ", status = list(enum = "correct"))
  ))
})

test_that("testDF rejects a data frame with another value", {
  judge <- local_judge_with(df = data.frame(id = 1:3, name = c("a", "b", "x")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    \n  3  c    ", description = "df"),
    list(command = "append-message", message = list(
      description = "Only the first five rows of the dataframes are shown. If they are all equal, your mistake only shows up in the later rows.",
      format = "plain"
    )),
    list(command = "close-test", generated = " id  name \n---  -----\n  1  a    \n  2  b    \n  3  x    ", status = list(enum = "wrong"))
  ))
})

test_that("testDF ignores the row order by default", {
  judge <- local_judge_with(df = data.frame(id = 1:3, name = c("a", "b", "c"))[c(3, 1, 2), ])

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")))
  })

  expect_equal(feedback[[3]], list(
    command = "close-test",
    generated = "      id  name \n---  ---  -----\n3      3  c    \n1      1  a    \n2      2  b    ",
    status = list(enum = "correct")
  ))
})

test_that("testDF checks the row order with ignore_row_order = FALSE", {
  judge <- local_judge_with(df = data.frame(id = 1:3, name = c("a", "b", "c"))[c(3, 1, 2), ])

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")), ignore_row_order = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "wrong"))
})

test_that("testDF ignores the column order by default", {
  judge <- local_judge_with(df = data.frame(name = c("a", "b", "c"), id = 1:3))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")))
  })

  expect_equal(feedback[[3]], list(
    command = "close-test",
    generated = "name    id\n-----  ---\na        1\nb        2\nc        3",
    status = list(enum = "correct")
  ))
})

test_that("testDF checks the column order with ignore_col_order = FALSE", {
  judge <- local_judge_with(df = data.frame(name = c("a", "b", "c"), id = 1:3))

  feedback <- capture_feedback(judge, {
    expected <- data.frame(id = 1:3, name = c("a", "b", "c"))
    testDF("any rows", function(env) env$df, expected, ignore_col_order = FALSE)
    testDF("ordered rows", function(env) env$df, expected, ignore_col_order = FALSE, ignore_row_order = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "wrong"))
  expect_equal(feedback[[6]]$status, list(enum = "wrong"))
})

test_that("testDF passes extra arguments to its comparison", {
  judge <- local_judge_with(df = data.frame(value = c(1.001, 2), id = 1:2))

  feedback <- capture_feedback(judge, {
    expected <- data.frame(value = c(1, 2), id = 1:2)
    testDF("default", function(env) env$df, expected)
    testDF("any rows", function(env) env$df, expected, tolerance = 0.01)
    testDF("ordered rows", function(env) env$df, expected, tolerance = 0.01, ignore_row_order = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "wrong"))
  expect_equal(feedback[[6]]$status, list(enum = "correct"))
  expect_equal(feedback[[9]]$status, list(enum = "correct"))
})

test_that("testDF passes the data frames to the comparator as they are", {
  judge <- local_judge_with(df = data.frame(name = c("c", "a", "b"), id = c(3L, 1L, 2L)))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")),
           comparator = function(generated, expected) {
             identical(names(generated), c("name", "id")) && identical(generated$id, c(3L, 1L, 2L)) &&
               identical(names(expected), c("id", "name"))
           })
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testDF uses the comparator instead of its own comparison", {
  judge <- local_judge_with(df = data.frame(id = 1:3, name = c("a", "b", "c")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")),
           comparator = function(generated, expected, ...) FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "wrong"))
})

test_that("testDF passes extra arguments to the comparator", {
  judge <- local_judge_with(df = data.frame(id = 1:2, name = c("a", "b")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")), missing_rows = 1,
           comparator = function(generated, expected, missing_rows) nrow(generated) == nrow(expected) - missing_rows)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testDF reports an error in generated as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) stop("boom"), data.frame(id = 1:2, name = c("a", "b")))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    ", description = "df"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "df"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testDF reports an error in the comparator as a runtime error", {
  judge <- local_judge_with(df = data.frame(id = 1:2, name = c("a", "b")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:2, name = c("a", "b")),
           comparator = function(generated, expected) stop("boom"))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    ", description = "df"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "df"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testDF reports a value that isn't a data frame as a runtime error", {
  judge <- local_judge_with(df = 1:2)

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:2, name = c("a", "b")))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    ", description = "df"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "df"),
    list(command = "close-test", generated = "argument 1 is not a vector", status = list(enum = "runtime error"))
  ))
})

test_that("testDF reports a comparator that returns NA as a runtime error", {
  judge <- local_judge_with(df = data.frame(id = 1:2, name = c("a", "b")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:2, name = c("a", "b")),
           comparator = function(generated, expected) NA)
  })

  expect_equal(
    tail(feedback, 1)[[1]],
    list(command = "close-test", generated = "missing value where TRUE/FALSE needed", status = list(enum = "runtime error"))
  )
  # Bug #95: testDF emits invalid feedback when a comparator doesn't return TRUE or FALSE
  expect_failure(expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    ", description = "df"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "df"),
    list(command = "close-test", generated = "missing value where TRUE/FALSE needed", status = list(enum = "runtime error"))
  )))
})

test_that("testDF shows as many rows as its message says", {
  judge <- local_judge_with(df = data.frame(id = 1:10, square = (1:10)^2))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:10, square = (1:10)^2))
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
  # Bug #96: testDF says it shows five rows but shows six
  expect_failure(expect_equal(feedback, list(
    list(command = "start-test", expected = " id   square\n---  -------\n  1        1\n  2        4\n  3        9\n  4       16\n  5       25", description = "df"),
    list(command = "append-message", message = list(description = "Only the first five rows of the dataframe are shown.", format = "plain")),
    list(command = "close-test", generated = " id   square\n---  -------\n  1        1\n  2        4\n  3        9\n  4       16\n  5       25", status = list(enum = "correct"))
  )))
})

test_that("testDF accepts the same one-column data frame", {
  judge <- local_judge_with(df = data.frame(x = 1:3))

  feedback <- capture_feedback(judge, testDF("df", function(env) env$df, data.frame(x = 1:3)))

  expect_equal(feedback[[1]], list(command = "start-test", expected = "   x\n ---\n   1\n   2\n   3", description = "df"))
  # Bug #107: testDF fails with a runtime error on one-column data frames
  expect_failure(expect_equal(feedback, list(
    list(command = "start-test", expected = "   x\n ---\n   1\n   2\n   3", description = "df"),
    list(command = "append-message", message = list(description = "Only the first five rows of the dataframe are shown.", format = "plain")),
    list(command = "close-test", generated = "   x\n ---\n   1\n   2\n   3", status = list(enum = "correct"))
  )))
})

test_that("testDF accepts a tibble with the same data as the expected data frame", {
  judge <- local_judge_with(df = tibble::tibble(id = 1:2, name = c("a", "b")))

  feedback <- capture_feedback(judge, {
    expected <- data.frame(id = 1:2, name = c("a", "b"))
    testDF("any rows", function(env) env$df, expected)
    testDF("ordered rows", function(env) env$df, expected, ignore_row_order = FALSE)
  })

  expect_length(feedback, 6)
  # Bug #116: testDF rejects a tibble with the same data as the expected data frame
  expect_failure(expect_equal(
    list(feedback[[3]]$status, feedback[[6]]$status),
    list(list(enum = "correct"), list(enum = "correct"))
  ))
})

test_that("testDF reports a warning in the comparator before it starts the test", {
  judge <- local_judge_with(df = data.frame(id = 1:2, name = c("a", "b")))

  feedback <- capture_feedback(judge, {
    testDF("df", function(env) env$df, data.frame(id = 1:2, name = c("a", "b")),
           comparator = function(generated, expected) {
             warning("careful")
             TRUE
           })
  })

  expect_equal(feedback, list(
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    ", description = "df"),
    list(command = "append-message", message = list(description = "Only the first five rows of the dataframe are shown.", format = "plain")),
    list(command = "close-test", generated = " id  name \n---  -----\n  1  a    \n  2  b    ", status = list(enum = "correct"))
  ))
})

test_that("testDF reports a warning in generated", {
  judge <- local_judge_with(df = data.frame(id = 1:2, name = c("a", "b")))

  feedback <- suppressWarnings(capture_feedback(judge, {
    testDF("df", function(env) {
      warning("careful")
      env$df
    }, data.frame(id = 1:2, name = c("a", "b")))
  }))

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "correct"))
  # Bug #91: Warnings and messages from student code never reach the feedback
  expect_failure(expect_equal(feedback, list(
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    ", description = "df"),
    list(command = "append-message", message = list(description = "Only the first five rows of the dataframe are shown.", format = "plain")),
    list(command = "close-test", generated = " id  name \n---  -----\n  1  a    \n  2  b    ", status = list(enum = "correct"))
  )))
})

test_that("testDF__inReview__ accepts the same data frame and shows its first rows", {
  judge <- local_judge_with(df = data.frame(id = 1:10, square = (1:10)^2))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:10, square = (1:10)^2))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id   square\n---  -------\n  1        1\n  2        4\n  3        9\n  4       16\n  5       25", description = "df"),
    list(command = "append-message", message = list(description = "5 of the 10 generated rows are shown.", format = "plain")),
    list(command = "close-test", generated = " id   square\n---  -------\n  1        1\n  2        4\n  3        9\n  4       16\n  5       25", status = list(enum = "correct"))
  ))
})

test_that("testDF__inReview__ shows the rows around the first wrong row", {
  judge <- local_judge()
  generated <- data.frame(id = 1:10, square = (1:10)^2)
  generated$square[7] <- 0
  judge$test_env$clean_env <- list2env(list(df = generated))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:10, square = (1:10)^2))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id   square\n---  -------\n  5       25\n  6       36\n  7       49\n  8       64\n  9       81", description = "df"),
    list(command = "append-message", message = list(description = "5 of the 10 generated rows are shown.", format = "plain")),
    list(command = "append-message", message = list(description = "unexpected row with rownumber: 7", format = "plain")),
    list(command = "append-message", message = list(
      description = "> Note: Row order does matter, your dataframe has to match the the expected dataframe exactly.",
      format = "markdown"
    )),
    list(command = "close-test", generated = " id   square\n---  -------\n  5       25\n  6       36\n  7        0\n  8       64\n  9       81", status = list(enum = "wrong"))
  ))
})

test_that("testDF__inReview__ shows rows_before_error and rows_after_error rows around the wrong row", {
  judge <- local_judge()
  generated <- data.frame(id = 1:10, square = (1:10)^2)
  generated$square[7] <- 0
  judge$test_env$clean_env <- list2env(list(df = generated))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:10, square = (1:10)^2),
                       rows_before_error = 1, rows_after_error = 1)
  })

  expect_equal(feedback[[1]]$expected, " id   square\n---  -------\n  6       36\n  7       49\n  8       64")
  expect_equal(feedback[[2]]$message$description, "3 of the 10 generated rows are shown.")
  expect_equal(feedback[[5]]$generated, " id   square\n---  -------\n  6       36\n  7        0\n  8       64")
})

test_that("testDF__inReview__ rejects a wrong row with rows_after_error = 0", {
  judge <- local_judge()
  generated <- data.frame(id = 1:10, square = (1:10)^2)
  generated$square[7] <- 0
  judge$test_env$clean_env <- list2env(list(df = generated))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:10, square = (1:10)^2), rows_after_error = 0)
  })

  # Bug #117: testDF__inReview__ accepts a wrong data frame when rows_after_error is 0
  expect_failure(expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "wrong")))
})

test_that("testDF__inReview__ notes that the row order doesn't matter with ignore_row_order = TRUE", {
  # Silences #109, pinned in test-utils-df.R.
  withr::local_options(lifecycle_verbosity = "quiet")
  judge <- local_judge_with(df = data.frame(id = 3:1, name = c("x", "b", "a")))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")), ignore_row_order = TRUE)
  })

  expect_equal(feedback[[3]], list(command = "append-message", message = list(
    description = "Your dataframe contains at least one row which is not found in the solution.",
    format = "plain"
  )))
  expect_equal(feedback[[4]], list(command = "append-message", message = list(
    description = "> Note: Row order doesn't matter, the expected dataframe is just one of the possible solutions.",
    format = "markdown"
  )))
  expect_equal(feedback[[5]]$status, list(enum = "wrong"))
})

test_that("testDF__inReview__ ignores the column order by default", {
  judge <- local_judge_with(df = data.frame(name = c("a", "b", "c"), id = 1:3))

  feedback <- capture_feedback(judge, {
    expected <- data.frame(id = 1:3, name = c("a", "b", "c"))
    testDF__inReview__("default", function(env) env$df, expected)
    testDF__inReview__("ordered columns", function(env) env$df, expected, ignore_col_order = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "wrong"))
})

test_that("testDF__inReview__ rejects a value that isn't a data frame", {
  judge <- local_judge_with(df = 1:3)

  feedback <- capture_feedback(judge, testDF__inReview__("df", function(env) env$df, data.frame(id = 1:3)))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = "We expected an object of the class \"data.frame\" but none was found.", status = list(enum = "wrong"))
  ))
})

test_that("testDF__inReview__ reports an error in generated as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testDF__inReview__("df", function(env) stop("boom"), data.frame(id = 1:3)))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = "df"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testDF__inReview__ uses the comparator instead of its own comparison", {
  judge <- local_judge_with(df = data.frame(id = 3:1, name = c("c", "b", "a")))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:3, name = c("a", "b", "c")),
                       comparator = function(generated, expected, ...) TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = " id  name \n---  -----\n  1  a    \n  2  b    \n  3  c    ", description = "df"),
    list(command = "append-message", message = list(description = "3 of the 3 generated rows are shown.", format = "plain")),
    list(command = "close-test", generated = " id  name \n---  -----\n  3  c    \n  2  b    \n  1  a    ", status = list(enum = "correct"))
  ))
})

test_that("testDF__inReview__ passes extra arguments to the comparator", {
  judge <- local_judge_with(df = data.frame(id = 1:3))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:3), verdict = TRUE,
                       comparator = function(generated, expected, verdict) verdict)
  })

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "correct"))
})

test_that("testDF__inReview__ adds no empty message when the comparator rejects the data frame", {
  judge <- local_judge_with(df = data.frame(id = 1:3))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:3), comparator = function(generated, expected, ...) FALSE)
  })
  messages <- vapply(
    Filter(function(command) command$command == "append-message", feedback),
    function(command) command$message$description,
    ""
  )

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "wrong"))
  # Bug #119: testDF__inReview__ adds an empty message when a comparator rejects the data frame
  expect_failure(expect_false("" %in% messages))
})

test_that("testDF__inReview__ reports a comparator that returns NA as a runtime error", {
  judge <- local_judge_with(df = data.frame(id = 1:3))

  feedback <- capture_feedback(judge, {
    testDF__inReview__("df", function(env) env$df, data.frame(id = 1:3), comparator = function(generated, expected, ...) NA)
  })

  expect_equal(
    tail(feedback, 1)[[1]],
    list(command = "close-test", generated = "missing value where TRUE/FALSE needed", status = list(enum = "runtime error"))
  )
  # Bug #118: testDF__inReview__ emits invalid feedback when a comparator doesn't return TRUE or FALSE
  expect_failure(expect_equal(
    vapply(feedback, function(command) command$command, ""),
    c("start-test", "append-message", "close-test", "start-test", "close-test")
  ))
})
