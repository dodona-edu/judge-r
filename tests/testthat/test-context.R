test_that("context wraps its testcases in start-context and close-context", {
  judge <- local_judge()
  local_student_code(judge, "x <- 42")

  feedback <- capture_feedback(judge, {
    context(testcaseAssert("x is 42", function(env) identical(env$x, 42)))
  })

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "start-testcase", description = "x is 42"),
    list(command = "close-testcase", accepted = TRUE),
    list(command = "close-context")
  ))
})

test_that("context runs preExec in the student's environment before the student code", {
  judge <- local_judge()
  local_student_code(judge, "doubled <- given * 2")

  capture_feedback(judge, context({}, preExec = {
    given <- 10
  }))

  expect_equal(judge$test_env$clean_env$doubled, 20)
})

test_that("context does not run preExec where the testcases run", {
  judge <- local_judge()
  local_student_code(judge, "")

  feedback <- capture_feedback(judge, {
    context(get_reporter()$add_message(exists("given")), preExec = {
      given <- 10
    })
  })

  expect_equal(feedback_messages(feedback), "FALSE")
})

test_that("context lets the testcases use the variables of the evaluation file", {
  judge <- local_judge()
  local_student_code(judge, "")

  feedback <- capture_feedback(judge, {
    expected <- 42
    context(get_reporter()$add_message(expected))
  })

  expect_equal(feedback_messages(feedback), "42")
})

test_that("context stores the value of the student code in evaluationResult", {
  judge <- local_judge()
  local_student_code(judge, c("x <- 40", "x + 2"))

  capture_feedback(judge, context({}))

  expect_equal(judge$test_env$clean_env$evaluationResult, 42)
})

test_that("context stores NULL in evaluationResult for an empty submission", {
  judge <- local_judge()
  local_student_code(judge, character())

  capture_feedback(judge, context({}))

  expect_true(exists("evaluationResult", envir = judge$test_env$clean_env, inherits = FALSE))
  expect_null(judge$test_env$clean_env$evaluationResult)
})

test_that("context discards what the student code prints", {
  judge <- local_judge()
  local_student_code(judge, c('print("hello")', 'cat("world\\n")', "42"))

  feedback <- capture_feedback(judge, context({}))

  expect_equal(feedback_commands(feedback), c("start-context", "close-context"))
})

test_that("context evaluates the student code in a fresh environment every time", {
  judge <- local_judge()
  local_student_code(judge, 'seen <- exists("seen", inherits = FALSE)')

  capture_feedback(judge, {
    context({})
    context({})
  })

  expect_false(judge$test_env$clean_env$seen)
})

test_that("student code does not see the variables of the evaluation file", {
  judge <- local_judge()
  local_student_code(judge, 'found <- exists("secret")')

  capture_feedback(judge, {
    secret <- "only for the tests"
    context({})
  })

  expect_false(judge$test_env$clean_env$found)
})

test_that("student code does not see the judge's functions", {
  judge <- local_judge()
  local_student_code(judge, 'found <- c(exists("start_judgement"), exists("testcaseAssert"), exists("student_code"))')

  capture_feedback(judge, context({}))

  expect_equal(judge$test_env$clean_env$found, c(FALSE, FALSE, FALSE))
})

test_that("student code does not see the packages the tests attach", {
  judge <- local_judge()
  withr::local_package("splines")
  local_student_code(judge, 'found <- exists("bs")')

  capture_feedback(judge, context({}))

  expect_false(judge$test_env$clean_env$found)
})

test_that("student code cannot call a function the tests called from a package they attach", {
  judge <- local_judge()
  withr::local_package("splines")
  local_student_code(judge, "found <- tryCatch(is.function(bs), error = function(e) FALSE)")

  capture_feedback(judge, {
    basis <- bs(1:10)
    context({})
  })

  # Bug #112: Student and test code can use each other's package functions once one of them called it
  expect_failure(expect_false(judge$test_env$clean_env$found))
})

test_that("student code cannot call a function the judge called from its packages", {
  judge <- local_judge()
  local_student_code(judge, "found <- tryCatch(is.function(toJSON), error = function(e) FALSE)")

  capture_feedback(judge, {
    get_reporter()$add_message("uses toJSON")
    context({})
  })

  # Bug #112: Student and test code can use each other's package functions once one of them called it
  expect_failure(expect_false(judge$test_env$clean_env$found))
})

test_that("student code can attach packages and use them", {
  judge <- local_judge()
  local_student_code(judge, c("library(stats4)", 'found <- exists("mle")'))

  capture_feedback(judge, context({}))

  expect_true(judge$test_env$clean_env$found)
})

test_that("test code does not see the packages the student attaches", {
  judge <- local_judge()
  local_student_code(judge, "library(stats4)")

  feedback <- capture_feedback(judge, {
    context(testcaseAssert("mle is hidden", function(env) !exists("mle")))
  })

  expect_equal(feedback[[3]], list(command = "close-testcase", accepted = TRUE))
})

test_that("test code cannot call a function the student called from a package they attach", {
  judge <- local_judge()
  local_student_code(judge, c("library(tools)", 'title <- toTitleCase("hello")'))

  feedback <- capture_feedback(judge, {
    context(testcaseAssert("toTitleCase is hidden", function(env) {
      inherits(try(toTitleCase, silent = TRUE), "try-error")
    }))
  })

  # Bug #112: Student and test code can use each other's package functions once one of them called it
  expect_failure(expect_equal(feedback[[3]], list(command = "close-testcase", accepted = TRUE)))
})

test_that("test code does not see the student's variables", {
  judge <- local_judge()
  local_student_code(judge, "student_answer <- 42")

  feedback <- capture_feedback(judge, {
    context(testcaseAssert("student_answer is hidden", function(env) !exists("student_answer")))
  })

  expect_equal(feedback[[3]], list(command = "close-testcase", accepted = TRUE))
})

test_that("what test code sees of a data frame attached in preExec does not depend on the student code", {
  judge <- local_judge()
  evaluation <- quote(context({
    testcaseAssert("passed", function(env) identical(env$passed, exam_score >= 10))
  }, preExec = {
    exam <- data.frame(exam_score = c(8, 12))
    attach(exam)
  }))

  local_student_code(judge, "passed <- exam_score >= 10")
  attached_name <- capture_feedback(with_values(judge, evaluation = evaluation), eval(evaluation))
  local_student_code(judge, "passed <- exam$exam_score >= 10")
  column <- capture_feedback(with_values(judge, evaluation = evaluation), eval(evaluation))

  # Bug #65: Strange behaviour with attach() in preExec
  expect_failure(expect_equal(attached_name, column))
})

test_that("context reports a syntax error in the student code as a compilation error", {
  judge <- local_judge()
  local_student_code(judge, "x <- (1")

  feedback <- capture_feedback(judge, context(testcaseAssert("not run", function(env) TRUE)))

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "append-message", message = list(
      description = "Error while evaluating context: <text>:2:0: unexpected end of input\n1: x <- (1\n   ^",
      format = "plain"
    )),
    list(command = "escalate-status", status = list(enum = "compilation error")),
    list(command = "close-context", accepted = FALSE)
  ))
})

test_that("context stops at an error in the student code before its testcases", {
  judge <- local_judge()
  local_student_code(judge, c("x <- undefined_name", "y <- 1"))

  feedback <- capture_feedback(judge, context(testcaseAssert("not run", function(env) TRUE)))

  expect_equal(feedback_commands(feedback), c("start-context", "append-message", "escalate-status", "close-context"))
  expect_equal(feedback_messages(feedback), "Error while evaluating context: object 'undefined_name' not found")
  expect_equal(feedback[[4]], list(command = "close-context", accepted = FALSE))
  expect_null(judge$test_env$clean_env$y)
})

test_that("context reports an error in the student code as a runtime error", {
  judge <- local_judge()
  local_student_code(judge, "x <- undefined_name")

  feedback <- capture_feedback(judge, context({}))

  expect_equal(feedback[[3]], list(command = "escalate-status", status = list(enum = "runtime error")))
})

test_that("context reports a warning from the student code", {
  judge <- local_judge()
  local_student_code(judge, 'warning("careful")')

  feedback <- suppressWarnings(capture_feedback(judge, context({})))

  # Bug #91: Warnings and messages from student code never reach the feedback
  expect_failure(expect_equal(feedback_messages(feedback), "Warning while evaluating context: careful"))
})

test_that("context reports a message from the student code", {
  judge <- local_judge()
  local_student_code(judge, 'message("just so you know")')

  feedback <- suppressMessages(capture_feedback(judge, context({})))

  # Bug #91: Warnings and messages from student code never reach the feedback
  expect_failure(expect_equal(feedback_messages(feedback), "Message while evaluating context: just so you know\n"))
})

test_that("context reports an error in preExec and skips the student code", {
  judge <- local_judge()
  local_student_code(judge, "ran <- TRUE")

  feedback <- capture_feedback(judge, context({}, preExec = stop("broken preExec")))

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "append-message", message = list(
      description = "Error while evaluating context: broken preExec",
      format = "plain"
    )),
    list(command = "escalate-status", status = list(enum = "compilation error")),
    list(command = "close-context", accepted = FALSE)
  ))
  expect_null(judge$test_env$clean_env$ran)
})

test_that("context reports an error in its testcases outside a testcase", {
  judge <- local_judge()
  local_student_code(judge, "")

  feedback <- capture_feedback(judge, context({
    get_reporter()$add_message("before the error")
    stop("broken test code")
  }))

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "append-message", message = list(description = "before the error", format = "plain")),
    list(command = "append-message", message = list(
      description = "Error while evaluating context: broken test code",
      format = "plain"
    )),
    list(command = "escalate-status", status = list(enum = "compilation error")),
    list(command = "close-context", accepted = FALSE)
  ))
})

test_that("context does not let preExec print into the feedback", {
  judge <- local_judge()
  local_student_code(judge, "")

  # Bug #49: stdout should be captured/discarded during test evaluation
  expect_failure(expect_no_error(
    capture_feedback(judge, context({}, preExec = print("debugging"))),
    message = "Not a feedback command"
  ))
})

test_that("context does not let its testcases print into the feedback", {
  judge <- local_judge()
  local_student_code(judge, "")

  # Bug #49: stdout should be captured/discarded during test evaluation
  expect_failure(expect_no_error(
    capture_feedback(judge, context(print("debugging"))),
    message = "Not a feedback command"
  ))
})

test_that("context restores the parent of the global environment", {
  judge <- local_judge()
  local_student_code(judge, "library(stats4)")
  parent <- parent.env(.GlobalEnv)

  capture_feedback(judge, context({}))

  expect_identical(parent.env(.GlobalEnv), parent)
})

test_that("context restores the parent of the global environment after an error", {
  judge <- local_judge()
  parent <- parent.env(.GlobalEnv)

  local_student_code(judge, "x <- (1")
  capture_feedback(judge, context({}))
  expect_identical(parent.env(.GlobalEnv), parent)

  local_student_code(judge, "stop('boom')")
  capture_feedback(judge, context({}))
  expect_identical(parent.env(.GlobalEnv), parent)

  local_student_code(judge, "")
  capture_feedback(judge, context({}, preExec = stop("boom")))
  expect_identical(parent.env(.GlobalEnv), parent)
})

test_that("contextWithRmd evaluates the R chunks and ignores the text", {
  judge <- local_judge()
  local_student_code(judge, c(
    "# Answer",
    "Here x <- 2 is just text.",
    "```{r}",
    "x <- 1",
    "```"
  ))

  feedback <- capture_feedback(judge, contextWithRmd({}))

  expect_equal(feedback_commands(feedback), c("start-context", "close-context"))
  expect_equal(judge$test_env$clean_env$x, 1)
})

test_that("contextWithRmd skips chunks with eval = FALSE", {
  judge <- local_judge()
  local_student_code(judge, c(
    "```{r}",
    "x <- 1",
    "```",
    "```{r, eval = FALSE}",
    "x <- 2",
    "```"
  ))

  capture_feedback(judge, contextWithRmd({}))

  expect_equal(judge$test_env$clean_env$x, 1)
})

test_that("contextWithRmd reports a syntax error in a chunk as a compilation error", {
  judge <- local_judge()
  local_student_code(judge, c("```{r}", "x <- (1", "```"))

  feedback <- capture_feedback(judge, contextWithRmd({}))

  expect_equal(feedback_commands(feedback), c("start-context", "append-message", "escalate-status", "close-context"))
  expect_match(feedback_messages(feedback), "^Error while evaluating context: <text>:[0-9]+:[0-9]+: unexpected")
  expect_equal(feedback[[3]], list(command = "escalate-status", status = list(enum = "compilation error")))
})

test_that("contextWithImage adds the plot of the student code after the testcases", {
  judge <- local_judge()
  local_student_code(judge, "plot(1)")

  feedback <- capture_feedback(judge, contextWithImage(testcaseAssert("plotted", function(env) TRUE)))

  expect_equal(
    feedback_commands(feedback),
    c("start-context", "start-testcase", "close-testcase", "append-message", "close-context")
  )
  expect_equal(feedback[[4]]$message$format, "html")
  expect_match(feedback[[4]]$message$description, '^<img src="data:image/png;base64,[A-Za-z0-9+/=]+"/>$')
})

test_that("contextWithImage passes extra arguments to png", {
  judge <- local_judge()
  local_student_code(judge, "plot(1)")

  feedback <- capture_feedback(judge, contextWithImage({}, width = 300, height = 200))

  payload <- sub('^<img src="data:image/png;base64,(.*)"/>$', "\\1", feedback_messages(feedback))
  image <- base64enc::base64decode(payload)
  expect_equal(image[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))
  # The PNG header holds the width and height as big-endian integers.
  expect_equal(readBin(image[17:24], "integer", n = 2, endian = "big"), c(300L, 200L))
})

test_that("contextWithImage fails the context when the student code draws nothing", {
  judge <- local_judge()
  local_student_code(judge, "x <- 1")

  feedback <- capture_feedback(judge, contextWithImage({}))

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "start-testcase", description = ""),
    list(command = "start-test", expected = "", description = ""),
    list(
      command = "close-test",
      generated = "We expected an image, but it doesn't seem like the code generated one.",
      status = list(enum = "wrong")
    ),
    list(command = "close-testcase"),
    list(command = "close-context")
  ))
})

test_that("contextWithImage with failIfAbsent = FALSE only adds a message when the student code draws nothing", {
  judge <- local_judge()
  local_student_code(judge, "x <- 1")

  feedback <- capture_feedback(judge, contextWithImage({}, failIfAbsent = FALSE))

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "append-message", message = list(
      description = "We expected an image, but it doesn't seem like the code generated one.",
      format = "plain"
    )),
    list(command = "close-context")
  ))
})

test_that("contextWithImage closes its graphics device, also after an error", {
  judge <- local_judge()
  device <- dev.cur()

  local_student_code(judge, "plot(1)")
  capture_feedback(judge, contextWithImage({}))
  expect_equal(dev.cur(), device)

  local_student_code(judge, c("plot(1)", "stop('boom')"))
  capture_feedback(judge, contextWithImage({}))
  expect_equal(dev.cur(), device)
})

test_that("contextWithImage adds the plot when the student code closes the device itself", {
  judge <- local_judge()
  # With another device open, the judge's dev.off() would close that one instead.
  expect_equal(names(dev.cur()), "null device")
  local_student_code(judge, c("plot(1)", "dev.off()"))

  feedback <- capture_feedback(judge, contextWithImage({}))

  expect_match(feedback_messages(feedback), '^<img src="data:image/png;base64,')
})
