test_that("start_reporter writes start-judgement", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$start_reporter()), '{"command": "start-judgement"}')
})

test_that("end_reporter writes close-judgement", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$end_reporter()), '{"command": "close-judgement"}')
})

test_that("start_tab writes start-tab with the title", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$start_tab("values")), '{"command": "start-tab", "title": "values"}')
})

test_that("start_tab escapes quotes, backslashes and control characters in the title", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$start_tab("a \"b\" \\ c\nd\te")),
    '{"command": "start-tab", "title": "a \\"b\\" \\\\ c\\nd\\te"}'
  )
})

test_that("start_tab writes UTF-8 characters as they are", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$start_tab("ℹ café")), '{"command": "start-tab", "title": "ℹ café"}')
})

test_that("start_tab writes a latin1 title as UTF-8", {
  reporter <- local_judge()$get_reporter()
  title <- iconv("café", "UTF-8", "latin1")

  line <- capture.output(reporter$start_tab(title))

  expect_equal(charToRaw(line), charToRaw('{"command": "start-tab", "title": "café"}'))
})

test_that("end_tab writes close-tab", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$end_tab()), '{"command": "close-tab"}')
})

test_that("start_context writes start-context", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$start_context()), '{"command": "start-context" }')
})

test_that("end_context writes close-context without accepted by default", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$end_context()), '{"command": "close-context"}')
})

test_that("end_context writes accepted when it is given", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$end_context(TRUE)), '{"command": "close-context", "accepted": true}')
  expect_equal(capture.output(reporter$end_context(FALSE)), '{"command": "close-context", "accepted": false}')
})

test_that("start_testcase writes start-testcase with the description", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$start_testcase("x is \"42\"")),
    '{"command": "start-testcase", "description": "x is \\"42\\""}'
  )
})

test_that("end_testcase writes close-testcase without accepted by default", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$end_testcase()), '{"command": "close-testcase"}')
})

test_that("end_testcase writes accepted when it is given", {
  reporter <- local_judge()$get_reporter()

  expect_equal(capture.output(reporter$end_testcase(TRUE)), '{"command": "close-testcase", "accepted": true}')
  expect_equal(capture.output(reporter$end_testcase(FALSE)), '{"command": "close-testcase", "accepted": false}')
})

test_that("start_test writes start-test with the expected value and description as text", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$start_test(42, "x")),
    '{"command": "start-test", "expected": "42", "description": "x"}'
  )
})

test_that("start_test joins vectors with a comma", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$start_test(c(1.5, 2), c("x", "y"))),
    '{"command": "start-test", "expected": "1.5, 2", "description": "x, y"}'
  )
})

test_that("start_test writes NULL as an empty string", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$start_test(NULL, NULL)),
    '{"command": "start-test", "expected": "", "description": ""}'
  )
})

test_that("end_test writes close-test with the generated value as text and the status", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$end_test(list(1, "a\"b"), "wrong")),
    '{"command": "close-test", "generated": "1, a\\"b", "status": {"enum": "wrong"}}'
  )
})

test_that("add_message writes a plain message by default", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$add_message("Only\n\"five\" rows")),
    '{"command": "append-message", "message": { "description": "Only\\n\\"five\\" rows", "format": "plain"} }'
  )
})

test_that("add_message writes the format it is given", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$add_message("**bold**", type = "markdown")),
    '{"command": "append-message", "message": { "description": "**bold**", "format": "markdown"} }'
  )
})

test_that("add_message joins a vector with a comma", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$add_message(c("a", "b"))),
    '{"command": "append-message", "message": { "description": "a, b", "format": "plain"} }'
  )
})

test_that("escalate writes escalate-status", {
  reporter <- local_judge()$get_reporter()

  expect_equal(
    capture.output(reporter$escalate("runtime error")),
    '{"command": "escalate-status", "status": {"enum": "runtime error"}}'
  )
})
