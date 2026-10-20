test_that("testFunctionUsed adds a message when the code calls the function", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "x <- c(1, 2)\nprint(round(mean(x)))")

  feedback <- capture_feedback(judge, testFunctionUsed("mean"))

  expect_length(feedback, 1)
  expect_equal(feedback[[1]]$command, "append-message")
  expect_equal(feedback[[1]], list(
    command = "append-message",
    message = list(description = "You used the \"mean\" function in your code.", format = "plain")
  ))
})

test_that("testFunctionUsed fails a test when the code doesn't call the function", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "x <- c(1, 2)\nsum(x)")

  feedback <- capture_feedback(judge, testFunctionUsed("mean"))

  expect_equal(feedback[[2]]$status, list(enum = "wrong"))
  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = "We expected you to use the \"mean\" function in your code.", status = list(enum = "wrong"))
  ))
})

test_that("testFunctionUsed doesn't count the function's name in a string", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "x <- 'mean'")

  feedback <- capture_feedback(judge, testFunctionUsed("mean"))

  expect_equal(feedback[[2]]$status, list(enum = "wrong"))
})

test_that("testFunctionUsedInVar adds a message when the assignment to the variable calls the function", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "x <- c(1, 2)\nm <- round(mean(x))")

  feedback <- capture_feedback(judge, testFunctionUsedInVar("mean", "m"))

  expect_equal(feedback, list(
    list(command = "append-message", message = list(
      description = "You used the \"mean\" function in an assignment to variable \"m\".",
      format = "plain"
    ))
  ))
})

test_that("testFunctionUsedInVar follows the variables the assignment uses", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "m <- mean(c(1, 2))\nresult <- m")

  feedback <- capture_feedback(judge, testFunctionUsedInVar("mean", "result"))

  expect_equal(feedback[[1]]$message$description, "You used the \"mean\" function in an assignment to variable \"result\".")
})

test_that("testFunctionUsedInVar finds the function in a chained assignment", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "a <- b <- mean(1)")

  feedback <- capture_feedback(judge, testFunctionUsedInVar("mean", "a"))

  expect_equal(feedback[[1]]$message$description, "You used the \"mean\" function in an assignment to variable \"a\".")
})

test_that("testFunctionUsedInVar accepts = and -> assignments", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "a = mean(1)\nmean(2) -> b")

  feedback <- capture_feedback(judge, {
    testFunctionUsedInVar("mean", "a")
    testFunctionUsedInVar("mean", "b")
  })

  expect_equal(feedback[[1]]$message$description, "You used the \"mean\" function in an assignment to variable \"a\".")
  expect_equal(feedback[[2]]$message$description, "You used the \"mean\" function in an assignment to variable \"b\".")
})

test_that("testFunctionUsedInVar accepts the function in any assignment to the variable", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "m <- mean(1)\nm <- 2")

  feedback <- capture_feedback(judge, testFunctionUsedInVar("mean", "m"))

  expect_equal(feedback[[1]]$command, "append-message")
})

test_that("testFunctionUsedInVar fails a test when only another variable's assignment calls the function", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "x <- mean(1)\nm <- 2")

  feedback <- capture_feedback(judge, testFunctionUsedInVar("mean", "m"))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = "We expected you to use the \"mean\" function in an assignment to variable \"m\".", status = list(enum = "wrong"))
  ))
})

test_that("testFunctionUsedInVar fails a test when the code never assigns the variable", {
  judge <- local_judge()
  judge$test_env$parsed_code <- parse(text = "x <- mean(1)")

  feedback <- capture_feedback(judge, testFunctionUsedInVar("mean", "m"))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = "Variable \"m\" not found", status = list(enum = "wrong"))
  ))
})
