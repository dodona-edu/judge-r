# The command the reporter wrote, parsed: whitespace and key order are not part of the format.
emitted <- function(lines) {
  expect_length(lines, 1)
  jsonlite::fromJSON(lines, simplifyVector = FALSE)
}

test_that("start_reporter writes start-judgement", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$start_reporter())), list(command = "start-judgement"))
})

test_that("end_reporter writes close-judgement", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_reporter())), list(command = "close-judgement"))
})

test_that("each command is written on one line", {
  reporter <- local_judge()$get_reporter()

  expect_length(capture.output(reporter$add_message("a\nb")), 1)
})

test_that("start_tab writes start-tab with the title", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$start_tab("values"))), list(command = "start-tab", title = "values"))
})

test_that("start_tab escapes quotes, backslashes and control characters in the title", {
  reporter <- local_judge()$get_reporter()
  title <- "a \"b\" \\ c\nd\te"

  line <- capture.output(reporter$start_tab(title))

  expect_equal(emitted(line)$title, title)
  expect_match(line, "a \\\"b\\\" \\\\ c\\nd\\te", fixed = TRUE)
})

test_that("start_tab writes UTF-8 characters as they are", {
  reporter <- local_judge()$get_reporter()

  line <- capture.output(reporter$start_tab("ℹ café"))

  expect_match(line, "ℹ café", fixed = TRUE)
  expect_equal(emitted(line)$title, "ℹ café")
})

test_that("start_tab writes a latin1 title as UTF-8", {
  reporter <- local_judge()$get_reporter()
  title <- iconv("café", "UTF-8", "latin1")

  line <- capture.output(reporter$start_tab(title))

  expect_true(grepl("café", line, fixed = TRUE, useBytes = TRUE))
})

test_that("end_tab writes close-tab", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_tab())), list(command = "close-tab"))
})

test_that("start_context writes start-context", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$start_context())), list(command = "start-context"))
})

test_that("end_context writes close-context without accepted by default", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_context())), list(command = "close-context"))
})

test_that("end_context writes accepted when it is given", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_context(TRUE))), list(command = "close-context", accepted = TRUE))
  expect_equal(emitted(capture.output(reporter$end_context(FALSE))), list(command = "close-context", accepted = FALSE))
})

test_that("start_testcase writes start-testcase with the description", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$start_testcase("x is \"42\""))),
    list(command = "start-testcase", description = "x is \"42\"")
  )
})

test_that("end_testcase writes close-testcase without accepted by default", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_testcase())), list(command = "close-testcase"))
})

test_that("end_testcase writes accepted when it is given", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_testcase(TRUE))), list(command = "close-testcase", accepted = TRUE))
  expect_equal(emitted(capture.output(reporter$end_testcase(FALSE))), list(command = "close-testcase", accepted = FALSE))
})

test_that("start_test writes start-test with the expected value and description as text", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$start_test(42, "x"))),
    list(command = "start-test", expected = "42", description = "x")
  )
})

test_that("start_test joins vectors with a comma", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$start_test(c(1.5, 2), c("x", "y")))),
    list(command = "start-test", expected = "1.5, 2", description = "x, y")
  )
})

test_that("start_test writes NULL as an empty string", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$start_test(NULL, NULL))),
    list(command = "start-test", expected = "", description = "")
  )
})

test_that("start_test writes an empty vector and an empty string as an empty string", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$start_test(character(0), ""))),
    list(command = "start-test", expected = "", description = "")
  )
})

test_that("start_test writes NA as the text NA", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$start_test(NA, NA_character_))),
    list(command = "start-test", expected = "NA", description = "NA")
  )
})

test_that("end_test writes close-test with the generated value as text and the status", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$end_test(list(1, "a\"b"), "wrong"))),
    list(command = "close-test", generated = "1, a\"b", status = list(enum = "wrong"))
  )
})

test_that("end_test writes NULL, NA and an empty string as text", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$end_test(NULL, "correct")))$generated, "")
  expect_equal(emitted(capture.output(reporter$end_test(NA, "correct")))$generated, "NA")
  expect_equal(emitted(capture.output(reporter$end_test("", "correct")))$generated, "")
})

test_that("end_test writes a string with quotes, newlines and UTF-8 characters unchanged", {
  reporter <- local_judge()$get_reporter()
  generated <- "\"é\"\n\\ ℹ\t\"x\""

  expect_equal(emitted(capture.output(reporter$end_test(generated, "wrong")))$generated, generated)
})

test_that("add_message writes a plain message by default", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$add_message("Only\n\"five\" rows"))),
    list(command = "append-message", message = list(description = "Only\n\"five\" rows", format = "plain"))
  )
})

test_that("add_message writes the format it is given", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$add_message("**bold**", type = "markdown"))),
    list(command = "append-message", message = list(description = "**bold**", format = "markdown"))
  )
})

test_that("add_message joins a vector with a comma", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$add_message(c("a", "b"))))$message$description,
    "a, b"
  )
})

test_that("add_message writes NULL as an empty description", {
  reporter <- local_judge()$get_reporter()

  expect_equal(emitted(capture.output(reporter$add_message(NULL)))$message$description, "")
})

test_that("escalate writes escalate-status", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    emitted(capture.output(reporter$escalate("runtime error"))),
    list(command = "escalate-status", status = list(enum = "runtime error"))
  )
})
