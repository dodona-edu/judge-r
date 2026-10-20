test_that("testcase wraps its tests in start-testcase and close-testcase", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcase("x", get_reporter()$add_message("inside")))

  expect_equal(feedback, list(
    list(command = "start-testcase", description = "x"),
    list(command = "append-message", message = list(description = "inside", format = "plain")),
    list(command = "close-testcase")
  ))
})

test_that("testcase without a description writes an empty description", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcase(tests = NULL))

  expect_equal(feedback[[1]], list(command = "start-testcase", description = ""))
})

test_that("testcase reports an error in its tests as a runtime error", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcase("x", stop("boom")))

  expect_equal(feedback, list(
    list(command = "start-testcase", description = "x"),
    list(command = "append-message", message = list(
      description = "Error while evaluating testcase: boom",
      format = "plain"
    )),
    list(command = "escalate-status", status = list(enum = "runtime error")),
    list(command = "close-testcase", accepted = FALSE)
  ))
})

test_that("testcase stops its tests at an error", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcase("x", {
    stop("boom")
    get_reporter()$add_message("after the error")
  }))

  expect_equal(feedback_messages(feedback), "Error while evaluating testcase: boom")
})

test_that("an error in a testcase does not stop the next testcase", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, {
    testcase("broken", stop("boom"))
    testcase("fine", NULL)
  })

  expect_equal(feedback[5:6], list(
    list(command = "start-testcase", description = "fine"),
    list(command = "close-testcase")
  ))
})

test_that("testcaseAssert accepts the testcase when the checker returns TRUE", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcaseAssert("x", function(env) TRUE))

  expect_equal(feedback, list(
    list(command = "start-testcase", description = "x"),
    list(command = "close-testcase", accepted = TRUE)
  ))
})

test_that("testcaseAssert rejects the testcase as wrong when the checker returns FALSE", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcaseAssert("x", function(env) FALSE))

  expect_equal(feedback, list(
    list(command = "start-testcase", description = "x"),
    list(command = "escalate-status", status = list(enum = "wrong")),
    list(command = "close-testcase", accepted = FALSE)
  ))
})

test_that("testcaseAssert passes the student's environment to the checker", {
  judge <- local_judge()
  local_student_code(judge, "x <- 42")

  feedback <- capture_feedback(judge, {
    context(testcaseAssert("x is 42", function(env) identical(env$x, 42)))
  })

  expect_equal(feedback[[3]], list(command = "close-testcase", accepted = TRUE))
})

test_that("testcaseAssert reports an error in the checker as a runtime error", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcaseAssert("x", function(env) stop("boom")))

  expect_equal(feedback, list(
    list(command = "start-testcase", description = "x"),
    list(command = "append-message", message = list(
      description = "Error while evaluating assert: boom",
      format = "plain"
    )),
    list(command = "escalate-status", status = list(enum = "runtime error")),
    list(command = "close-testcase", accepted = FALSE)
  ))
})

test_that("testcaseAssert discards what the checker prints", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, testcaseAssert("x", function(env) {
    print("debugging")
    TRUE
  }))

  expect_equal(feedback_commands(feedback), c("start-testcase", "close-testcase"))
})

test_that("testcaseAssert reports a warning from the checker", {
  judge <- local_judge()

  feedback <- suppressWarnings(capture_feedback(judge, testcaseAssert("x", function(env) {
    warning("careful")
    TRUE
  })))

  expect_equal(feedback_messages(feedback), "Warning while evaluating assert: careful")
})
