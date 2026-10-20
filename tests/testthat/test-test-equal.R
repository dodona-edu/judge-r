test_that("testEqual accepts a value that all.equal accepts", {
  judge <- local_judge_with(x = 1 + 1e-10)

  feedback <- capture_feedback(judge, testEqual("x", function(env) env$x, 1))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "1.0000000001", status = list(enum = "correct"))
  ))
})

test_that("testEqual rejects another value", {
  judge <- local_judge_with(x = 2)

  feedback <- capture_feedback(judge, testEqual("x", function(env) env$x, 1))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "2", status = list(enum = "wrong"))
  ))
})

test_that("testEqual shows a vector as comma-separated values", {
  judge <- local_judge_with(x = c(1, 3))

  feedback <- capture_feedback(judge, testEqual("x", function(env) env$x, c(1, 2)))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1, 2", description = "x"),
    list(command = "close-test", generated = "1, 3", status = list(enum = "wrong"))
  ))
})

test_that("testEqual passes extra arguments to all.equal", {
  judge <- local_judge_with(x = 1.05)

  feedback <- capture_feedback(judge, testEqual("x", function(env) env$x, 1, tolerance = 0.1))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "1.05", status = list(enum = "correct"))
  ))
})

test_that("testEqual uses the comparator instead of all.equal", {
  judge <- local_judge_with(x = 2)

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) env$x, 1, comparator = function(generated, expected, ...) generated > expected)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "2", status = list(enum = "correct"))
  ))
})

test_that("testEqual passes extra arguments to the comparator", {
  judge <- local_judge_with(x = 2)

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) env$x, 1, offset = 1,
              comparator = function(generated, expected, offset) generated == expected + offset)
  })

  expect_equal(feedback[[2]], list(command = "close-test", generated = "2", status = list(enum = "correct")))
})

test_that("testEqual shows the messages a comparator adds inside the test", {
  judge <- local_judge_with(x = "TRUE")

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) env$x, TRUE, comparator = function(generated, expected, ...) {
      if (isTRUE(all.equal(generated, "TRUE"))) {
        get_reporter()$add_message("You should not add quotes around `TRUE`", type = "markdown")
      }
      isTRUE(all.equal(generated, expected, ...))
    })
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "TRUE", description = "x"),
    list(command = "append-message", message = list(description = "You should not add quotes around `TRUE`", format = "markdown")),
    list(command = "close-test", generated = "TRUE", status = list(enum = "wrong"))
  ))
})

test_that("testEqual shows both values through the formatter", {
  judge <- local_judge_with(x = 2)

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) env$x, 1, formatter = function(value) sprintf("%.2f", value))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1.00", description = "x"),
    list(command = "close-test", generated = "2.00", status = list(enum = "wrong"))
  ))
})

test_that("testEqual compares the values, not what the formatter makes of them", {
  judge <- local_judge_with(x = 1.001)

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) env$x, 1, formatter = function(value) sprintf("%.1f", value))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1.0", description = "x"),
    list(command = "close-test", generated = "1.0", status = list(enum = "wrong"))
  ))
})

test_that("testEqual hides what generated prints", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) {
      print("printed")
      cat("written\n")
      1
    }, 1)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "1", status = list(enum = "correct"))
  ))
})

test_that("testEqual reports an error in generated as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testEqual("x", function(env) stop("boom"), 1))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testEqual reports an error in the comparator as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) 1, 1, comparator = function(generated, expected) stop("boom"))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testEqual reports a comparator that returns NA as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) 1, 1, comparator = function(generated, expected) NA)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "missing value where TRUE/FALSE needed", status = list(enum = "runtime error"))
  ))
})

test_that("testEqual reports an error formatting the generated value as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) "1", 1, formatter = function(value) {
      if (is.character(value)) stop("not a number")
      value
    })
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "not a number", status = list(enum = "runtime error"))
  ))
})

test_that("testEqual leaves an error formatting the expected value to the enclosing testcase", {
  judge <- local_judge_with()

  expect_error(
    capture_feedback(judge, testEqual("x", function(env) 1, 1, formatter = function(value) stop("boom"))),
    "boom"
  )
})

test_that("testEqual reports a warning in the comparator inside the test", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testEqual("x", function(env) 1, 1, comparator = function(generated, expected) {
      warning("careful")
      TRUE
    })
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "close-test", generated = "1", status = list(enum = "correct"))
  ))
})

test_that("testEqual reports a message in the comparator inside the test", {
  judge <- local_judge_with()

  feedback <- suppressMessages(capture_feedback(judge, {
    testEqual("x", function(env) 1, 1, comparator = function(generated, expected) {
      message("note")
      TRUE
    })
  }))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "append-message", message = list(description = "Message while evaluating test: note\n", format = "plain")),
    list(command = "close-test", generated = "1", status = list(enum = "correct"))
  ))
})

test_that("testEqual reports a warning in generated inside the test", {
  judge <- local_judge_with()

  feedback <- suppressWarnings(capture_feedback(judge, {
    testEqual("x", function(env) {
      warning("careful")
      1
    }, 1)
  }))

  expect_equal(tail(feedback, 1)[[1]], list(command = "close-test", generated = "1", status = list(enum = "correct")))
  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "close-test", generated = "1", status = list(enum = "correct"))
  ))
})

test_that("testIdentical rejects values that are equal but not identical", {
  judge <- local_judge_with(x = 1L)

  feedback <- capture_feedback(judge, testIdentical("x", function(env) env$x, 1))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "1", status = list(enum = "wrong"))
  ))
})

test_that("testIdentical accepts identical values", {
  judge <- local_judge_with(x = c(a = 1L))

  feedback <- capture_feedback(judge, testIdentical("x", function(env) env$x, c(a = 1L)))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "1", status = list(enum = "correct"))
  ))
})

test_that("testIdentical passes extra arguments to identical", {
  judge <- local_judge_with(x = -0)

  feedback <- capture_feedback(judge, {
    testIdentical("default", function(env) env$x, 0)
    testIdentical("num.eq", function(env) env$x, 0, num.eq = FALSE)
  })

  expect_equal(feedback[[2]], list(command = "close-test", generated = "0", status = list(enum = "correct")))
  expect_equal(feedback[[4]], list(command = "close-test", generated = "0", status = list(enum = "wrong")))
})

test_that("testIdentical shows both values through the formatter", {
  judge <- local_judge_with(x = 1L)

  feedback <- capture_feedback(judge, testIdentical("x", function(env) env$x, 1, formatter = class))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "numeric", description = "x"),
    list(command = "close-test", generated = "integer", status = list(enum = "wrong"))
  ))
})

test_that("testIdentical reports an error in generated as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testIdentical("x", function(env) stop("boom"), 1))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})
