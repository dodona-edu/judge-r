test_that(".add_condition_message names the kind, what was evaluated and the condition's message", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .add_condition_message("Warning", "test", simpleWarning("careful")))

  expect_equal(feedback, list(
    list(command = "append-message", message = list(
      description = "Warning while evaluating test: careful",
      format = "plain"
    ))
  ))
})

test_that(".report_conditions returns the value of the expression", {
  judge <- local_judge()

  expect_equal(judge$.report_conditions("test", 1 + 1), 2)
})

test_that(".report_conditions reports a warning and continues", {
  judge <- local_judge()

  feedback <- suppressWarnings(capture_feedback(judge, .report_conditions("test", {
    warning("careful")
    get_reporter()$add_message("after the warning")
  })))

  expect_equal(feedback_messages(feedback), c("Warning while evaluating test: careful", "after the warning"))
})

test_that(".report_conditions reports a message and continues", {
  judge <- local_judge()

  feedback <- suppressMessages(capture_feedback(judge, .report_conditions("test", {
    message("just so you know")
    get_reporter()$add_message("after the message")
  })))

  expect_equal(feedback_messages(feedback), c("Message while evaluating test: just so you know\n", "after the message"))
})

test_that(".report_conditions leaves errors to the caller", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, try(.report_conditions("test", stop("boom")), silent = TRUE))

  expect_equal(feedback, list())
})

test_that(".report_conditions reports a warning once when it is nested", {
  judge <- local_judge()

  feedback <- suppressWarnings(capture_feedback(judge, {
    .report_conditions("context", .report_conditions("test", warning("careful")))
  }))

  expect_equal(feedback_messages(feedback), "Warning while evaluating test: careful")
})

test_that(".fail_testcase reports the error and closes the testcase as a runtime error", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .fail_testcase("testcase", simpleError("boom")))

  expect_equal(feedback, list(
    list(command = "append-message", message = list(
      description = "Error while evaluating testcase: boom",
      format = "plain"
    )),
    list(command = "escalate-status", status = list(enum = "runtime error")),
    list(command = "close-testcase", accepted = FALSE)
  ))
})

test_that(".report_test_runtime_error adds a test with the error as a runtime error", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .report_test_runtime_error("x", simpleError("boom")))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that(".close_test_with_runtime_error closes the open test as wrong and adds the error as a test", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .close_test_with_runtime_error("x", simpleError("boom")))

  expect_equal(feedback, list(
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "x"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that(".report_image adds an existing file as an html image", {
  judge <- local_judge()
  file <- withr::local_tempfile(fileext = ".png")
  writeBin(as.raw(c(1, 2, 3)), file)

  feedback <- capture_feedback(
    with_values(judge, file = file),
    .report_image(file, fail_if_absent = TRUE, within_testcase = FALSE)
  )

  expect_equal(feedback, list(
    list(command = "append-message", message = list(
      description = '<img src="data:image/png;base64,AQID"/>',
      format = "html"
    ))
  ))
})

test_that(".report_image fails a testcase of its own when the image is absent", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .report_image(tempfile(), fail_if_absent = TRUE, within_testcase = FALSE))

  expect_equal(feedback, list(
    list(command = "start-testcase", description = ""),
    list(command = "start-test", expected = "", description = ""),
    list(
      command = "close-test",
      generated = "We expected an image, but it doesn't seem like the code generated one.",
      status = list(enum = "wrong")
    ),
    list(command = "close-testcase")
  ))
})

test_that(".report_image fails a test in the open testcase when the image is absent", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .report_image(tempfile(), fail_if_absent = TRUE, within_testcase = TRUE))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(
      command = "close-test",
      generated = "We expected an image, but it doesn't seem like the code generated one.",
      status = list(enum = "wrong")
    )
  ))
})

test_that(".report_image only adds a message when an absent image may be absent", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, .report_image(tempfile(), fail_if_absent = FALSE, within_testcase = FALSE))

  expect_equal(feedback, list(
    list(command = "append-message", message = list(
      description = "We expected an image, but it doesn't seem like the code generated one.",
      format = "plain"
    ))
  ))
})
