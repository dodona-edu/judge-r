test_that("local_judge gives every judge its own reporter and test_env", {
  first <- local_judge()
  second <- local_judge()

  expect_false(identical(first$get_reporter(), second$get_reporter()))
  expect_false(identical(first$test_env, second$test_env))
})

test_that("capture_feedback returns the reporter's commands, parsed", {
  judge <- local_judge()

  feedback <- capture_feedback(judge, {
    get_reporter()$start_tab("Tab")
    get_reporter()$end_tab()
  })

  expect_equal(feedback, list(list(command = "start-tab", title = "Tab"), list(command = "close-tab")))
})

test_that("capture_feedback rejects output that is not a command", {
  judge <- local_judge()

  expect_error(capture_feedback(judge, cat("hello\n")), "Not a feedback command: hello")
})

test_that("local_student_code is the submission a context evaluates", {
  judge <- local_judge()
  local_student_code(judge, "x <- 1")

  feedback <- capture_feedback(judge, {
    context({
      testcase("x", {
        testEqual("x", function(env) env$x, 1)
      })
    })
  })

  expect_equal(feedback, list(
    list(command = "start-context"),
    list(command = "start-testcase", description = "x"),
    list(command = "start-test", expected = "1", description = "x"),
    list(command = "close-test", generated = "1", status = list(enum = "correct")),
    list(command = "close-testcase"),
    list(command = "close-context")
  ))
})

test_that("student code does not see the packages the judge attaches", {
  judge <- local_judge()
  local_student_code(judge, "found <- exists('toJSON')")

  feedback <- capture_feedback(judge, {
    context({
      testcase("toJSON", {
        testEqual("found", function(env) env$found, FALSE)
      })
    })
  })

  expect_equal(feedback[[4]], list(command = "close-test", generated = "FALSE", status = list(enum = "correct")))
})

test_that("judge_packages are the packages run attaches", {
  run <- readLines(test_path("..", "..", "run"))
  attached <- sub("^\\s*library\\(['\"]?([^'\")]+)['\"]?\\).*$", "\\1", grep("^\\s*library\\(", run, value = TRUE))

  expect_equal(attached, judge_packages)

  lazy <- grep("for \\(package in c\\(", run, value = TRUE)
  expect_equal(eval(parse(text = sub("^.*for \\(package in (c\\([^)]*\\))\\).*$", "\\1", lazy))), judge_lazy_packages)
})
