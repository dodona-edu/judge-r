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

test_that("start_judgement opens and closes the judgement around a tab per evaluation file", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines("", file.path(tests, "01-values.R"))
  writeLines("", file.path(tests, "02-functions.R"))

  feedback <- capture_feedback(with_values(judge, tests = tests), start_judgement(tests, "submission.R"))

  expect_equal(feedback, list(
    list(command = "start-judgement"),
    list(command = "start-tab", title = "values"),
    list(command = "close-tab"),
    list(command = "start-tab", title = "functions"),
    list(command = "close-tab"),
    list(command = "close-judgement")
  ))
})

test_that("start_judgement orders the tabs by file name", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  for (file in c("2-second.R", "10-third.R", "01-first.R")) {
    writeLines("", file.path(tests, file))
  }

  feedback <- capture_feedback(with_values(judge, tests = tests), start_judgement(tests, "submission.R"))

  tabs <- Filter(function(command) command$command == "start-tab", feedback)
  expect_equal(vapply(tabs, function(tab) tab$title, ""), c("first", "third", "second"))
})

test_that("start_judgement evaluates the contexts of an evaluation file against the submission", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines(
    'context({ testcaseAssert("x is 42", function(env) identical(env$x, 42)) })',
    file.path(tests, "values.R")
  )
  submission <- withr::local_tempfile(lines = "x <- 42")

  feedback <- capture_feedback(
    with_values(judge, tests = tests, submission = submission),
    start_judgement(tests, submission)
  )

  expect_equal(feedback, list(
    list(command = "start-judgement"),
    list(command = "start-tab", title = "values"),
    list(command = "start-context"),
    list(command = "start-testcase", description = "x is 42"),
    list(command = "close-testcase", accepted = TRUE),
    list(command = "close-context"),
    list(command = "close-tab"),
    list(command = "close-judgement")
  ))
})

test_that("start_judgement points student_code at the submission", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines("get_reporter()$add_message(student_code)", file.path(tests, "path.R"))

  feedback <- capture_feedback(with_values(judge, tests = tests), start_judgement(tests, "/submission/source"))

  expect_equal(feedback_messages(feedback), "/submission/source")
})

test_that("start_judgement evaluates every evaluation file in its own environment", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines("shared <- 1", file.path(tests, "01-first.R"))
  writeLines('get_reporter()$add_message(exists("shared"))', file.path(tests, "02-second.R"))

  feedback <- capture_feedback(with_values(judge, tests = tests), start_judgement(tests, "submission.R"))

  expect_equal(feedback_messages(feedback), "FALSE")
})

test_that("start_judgement closes the tab and the judgement when an evaluation file errors", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines('stop("broken evaluation file")', file.path(tests, "broken.R"))

  feedback <- capture_feedback(
    with_values(judge, tests = tests),
    try(start_judgement(tests, "submission.R"), silent = TRUE)
  )

  expect_equal(feedback, list(
    list(command = "start-judgement"),
    list(command = "start-tab", title = "broken"),
    list(command = "close-tab"),
    list(command = "close-judgement")
  ))
})

test_that("start_judgement passes on the error of an evaluation file", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines('stop("broken evaluation file")', file.path(tests, "broken.R"))

  expect_error(capture.output(judge$start_judgement(tests, "submission.R")), "broken evaluation file")
})

test_that("start_judgement skips the evaluation files after one that errors", {
  judge <- local_judge()
  tests <- withr::local_tempdir()
  writeLines('stop("broken evaluation file")', file.path(tests, "01-broken.R"))
  writeLines("", file.path(tests, "02-fine.R"))

  feedback <- capture_feedback(
    with_values(judge, tests = tests),
    try(start_judgement(tests, "submission.R"), silent = TRUE)
  )

  expect_equal(feedback_commands(feedback), c("start-judgement", "start-tab", "close-tab", "close-judgement"))
})

test_that("install.packages() in the student code warns instead of installing", {
  judge <- local_judge()
  # No packages, so the real install.packages() does nothing.
  local_student_code(judge, 'reported <- tryCatch({ install.packages(character()); "nothing" }, warning = conditionMessage)')

  capture_feedback(judge, context({}))

  # Bug #87: Ignore when evaluation or student code runs install.packages
  expect_failure(expect_equal(
    judge$test_env$clean_env$reported,
    "install.packages() is not available on Dodona; installed packages can be loaded with library()"
  ))
})

test_that("install.packages() in the evaluation code warns instead of installing", {
  judge <- local_judge()
  local_student_code(judge, "")

  # No packages, so the real install.packages() does nothing.
  feedback <- capture_feedback(judge, context(install.packages(character())))

  # Bug #87: Ignore when evaluation or student code runs install.packages
  expect_failure(expect_equal(
    feedback_messages(feedback),
    "Warning while evaluating context: install.packages() is not available on Dodona; installed packages can be loaded with library()"
  ))
})
