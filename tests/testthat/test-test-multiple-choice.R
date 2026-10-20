test_that("testMultipleChoice accepts any valid option without verify_answer", {
  judge <- local_judge_with(answer = "mean")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "append-message", message = list(description = "Your solution will be verified after the deadline.", format = "plain")),
    list(command = "close-test", generated = "mean", status = list(enum = "correct"))
  ))
})

test_that("testMultipleChoice rejects an answer that isn't in possible_answers", {
  judge <- local_judge_with(answer = c("mean", "average"))

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "append-message", message = list(
      description = "Your answer is not a valid option, the valid options are (mean, median, mode).",
      format = "plain"
    )),
    list(command = "close-test", generated = "mean, average", status = list(enum = "wrong"))
  ))
})

test_that("testMultipleChoice rejects an empty answer", {
  judge <- local_judge_with(answer = character())

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"))
  })

  expect_equal(feedback[[2]]$message$description, "Your answer is not a valid option, the valid options are (mean, median, mode).")
  expect_equal(feedback[[3]], list(command = "close-test", generated = "", status = list(enum = "wrong")))
})

test_that("testMultipleChoice accepts possible_answers as a list", {
  judge <- local_judge_with(answer = "median")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", list("mean", "median", "mode"))
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testMultipleChoice accepts the expected option with verify_answer = TRUE", {
  judge <- local_judge_with(answer = "median")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"), verify_answer = TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "close-test", generated = "median", status = list(enum = "correct"))
  ))
})

test_that("testMultipleChoice accepts the expected options in any order and with duplicates", {
  judge <- local_judge_with(answer = c("mode", "median", "mode"))

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, c("median", "mode"), c("mean", "median", "mode"), verify_answer = TRUE)
  })

  expect_equal(feedback[[2]], list(command = "close-test", generated = "mode, median, mode", status = list(enum = "correct")))
})

test_that("testMultipleChoice names a wrong option with verify_answer = TRUE", {
  judge <- local_judge_with(answer = "mean")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"), verify_answer = TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "append-message", message = list(
      description = "\nmean is not a right answer.\nYour answer does not include all the correct options.",
      format = "plain"
    )),
    list(command = "close-test", generated = "mean", status = list(enum = "wrong"))
  ))
})

test_that("testMultipleChoice names every wrong option of an answer with extra options", {
  judge <- local_judge_with(answer = c("mode", "median", "mean"))

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"), verify_answer = TRUE)
  })

  expect_equal(feedback[[2]]$message$description, "\nmean is not a right answer.\nmode is not a right answer.")
  expect_equal(feedback[[3]]$status, list(enum = "wrong"))
})

test_that("testMultipleChoice says when an answer misses an expected option", {
  judge <- local_judge_with(answer = "mode")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, c("median", "mode"), c("mean", "median", "mode"), verify_answer = TRUE)
  })

  expect_equal(feedback[[2]]$message$description, "\nYour answer does not include all the correct options.")
  expect_equal(feedback[[3]]$status, list(enum = "wrong"))
})

test_that("testMultipleChoice explains a wrong option with its feedback", {
  judge <- local_judge_with(answer = "mean")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"), verify_answer = TRUE,
                       feedback = list(mean = "outliers pull it along"))
  })

  expect_equal(
    feedback[[2]]$message$description,
    "\nmean is not a right answer because: outliers pull it along\nYour answer does not include all the correct options."
  )
})

test_that("testMultipleChoice looks up the feedback of numeric options by position", {
  judge <- local_judge_with(answer = c(3, 1))

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, 2, 1:3, verify_answer = TRUE,
                       feedback = list("one is too small", "two is right", "three is too big"))
  })

  expect_equal(
    feedback[[2]]$message$description,
    "\n1 is not a right answer because: one is too small\n3 is not a right answer because: three is too big\nYour answer does not include all the correct options."
  )
  expect_equal(feedback[[3]], list(command = "close-test", generated = "3, 1", status = list(enum = "wrong")))
})

test_that("testMultipleChoice leaves out the explanation with give_feedback = FALSE", {
  judge <- local_judge_with(answer = "mean")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"), verify_answer = TRUE,
                       give_feedback = FALSE, feedback = list(mean = "outliers pull it along"))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "close-test", generated = "mean", status = list(enum = "wrong"))
  ))
})

test_that("testMultipleChoice shows the expected option with show_expected = TRUE", {
  judge <- local_judge_with(answer = "median")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, "median", c("mean", "median", "mode"), show_expected = TRUE)
  })

  expect_equal(feedback[[1]], list(command = "start-test", expected = "median", description = "q"))
})

test_that("testMultipleChoice shows every expected option with show_expected = TRUE", {
  judge <- local_judge_with(answer = "median")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) env$answer, c("median", "mode"), c("mean", "median", "mode"), show_expected = TRUE)
  })

  expect_equal(feedback[[1]]$command, "start-test")
  # Bug #99: testMultipleChoice with show_expected shows only the first expected answer
  expect_failure(expect_equal(feedback[[1]]$expected, "median, mode"))
})

test_that("testMultipleChoice reports an error in generated as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) stop("boom"), "median", c("mean", "median", "mode"))
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "q"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testMultipleChoice reports a warning in generated", {
  judge <- local_judge_with(answer = "median")

  feedback <- capture_feedback(judge, {
    testMultipleChoice("q", function(env) {
      warning("careful")
      env$answer
    }, "median", c("mean", "median", "mode"), verify_answer = TRUE)
  })

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "correct"))
  expect_equal(feedback, list(
    list(command = "start-test", expected = "●", description = "q"),
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "close-test", generated = "median", status = list(enum = "correct"))
  ))
})
