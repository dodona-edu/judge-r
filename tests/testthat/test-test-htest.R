# capture_feedback() runs its code in the judge: the expected value is bound there.

test_that("testHtest compares the p-value and the confidence interval by default", {
  judge <- local_judge_with(test = new_htest())
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\np-value = 0.04\nconfidence interval = 1, 2", description = "test"),
    list(command = "close-test", generated = "\np-value = 0.04\nconfidence interval = 1, 2", status = list(enum = "correct"))
  ))
})

test_that("testHtest rejects another p-value", {
  judge <- local_judge_with(test = new_htest(p.value = 0.05))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\np-value = 0.04\nconfidence interval = 1, 2", description = "test"),
    list(command = "close-test", generated = "\np-value = 0.05\nconfidence interval = 1, 2", status = list(enum = "wrong"))
  ))
})

test_that("testHtest rejects another confidence interval", {
  judge <- local_judge_with(test = new_htest(conf.int = c(1, 3)))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected))

  expect_equal(feedback[[2]], list(
    command = "close-test",
    generated = "\np-value = 0.04\nconfidence interval = 1, 3",
    status = list(enum = "wrong")
  ))
})

test_that("testHtest ignores the p-value with test_p_value = FALSE", {
  judge <- local_judge_with(test = new_htest(p.value = 0.05))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected, test_p_value = FALSE))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\nconfidence interval = 1, 2", description = "test"),
    list(command = "close-test", generated = "\nconfidence interval = 1, 2", status = list(enum = "correct"))
  ))
})

test_that("testHtest ignores the confidence interval with test_interval = FALSE", {
  judge <- local_judge_with(test = new_htest(conf.int = c(1, 3)))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected, test_interval = FALSE))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\np-value = 0.04", description = "test"),
    list(command = "close-test", generated = "\np-value = 0.04", status = list(enum = "correct"))
  ))
})

test_that("testHtest ignores the statistic, alternative and method by default", {
  judge <- local_judge_with(
    test = new_htest(statistic = c(t = 3), alternative = "less", method = "Paired t-test")
  )
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected))

  expect_equal(feedback[[2]]$status, list(enum = "correct"))
})

test_that("testHtest compares the test statistic with test_statistic = TRUE", {
  judge <- local_judge_with(test = new_htest(statistic = c(t = 3)))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, {
    testHtest("test", function(env) env$test, expected,
              test_p_value = FALSE, test_interval = FALSE, test_statistic = TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\ntest statistic = \n\tt: 2.5", description = "test"),
    list(command = "close-test", generated = "\ntest statistic = \n\tt: 3", status = list(enum = "wrong"))
  ))
})

test_that("testHtest compares the alternative with test_alternative = TRUE", {
  judge <- local_judge_with(test = new_htest(alternative = "less"))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, {
    testHtest("test", function(env) env$test, expected,
              test_p_value = FALSE, test_interval = FALSE, test_alternative = TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\nalternative = two.sided", description = "test"),
    list(command = "close-test", generated = "\nalternative = less", status = list(enum = "wrong"))
  ))
})

test_that("testHtest compares the confidence level with test_confidence_level = TRUE", {
  judge <- local_judge_with(test = new_htest(conf.level = 0.9))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, {
    testHtest("test", function(env) env$test, expected,
              test_p_value = FALSE, test_interval = FALSE, test_confidence_level = TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\nconfidence level = 0.95", description = "test"),
    list(command = "close-test", generated = "\nconfidence level = 0.9", status = list(enum = "wrong"))
  ))
})

test_that("testHtest compares the method with test_method = TRUE", {
  judge <- local_judge_with(test = new_htest(method = "Paired t-test"))
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, {
    testHtest("test", function(env) env$test, expected,
              test_p_value = FALSE, test_interval = FALSE, test_method = TRUE)
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "\nmethod = Welch Two Sample t-test", description = "test"),
    list(command = "close-test", generated = "\nmethod = Paired t-test", status = list(enum = "wrong"))
  ))
})

test_that("testHtest accepts the result of the same t.test", {
  judge <- local_judge_with(test = t.test(c(5.1, 4.9, 6.2, 5.8), c(4.1, 5.0, 4.6, 4.9)))

  feedback <- capture_feedback(judge, {
    testHtest("test", function(env) env$test, t.test(c(5.1, 4.9, 6.2, 5.8), c(4.1, 5.0, 4.6, 4.9)),
              test_statistic = TRUE, test_alternative = TRUE, test_confidence_level = TRUE, test_method = TRUE)
  })

  expect_equal(feedback[[2]]$status, list(enum = "correct"))
})

test_that("testHtest rejects a value that isn't an htest", {
  judge <- local_judge_with(test = 0.04)
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) env$test, expected))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = paste0(
      "We expected an object of the class \"htest\" but none was found. This is the class of objects ",
      "generated by functions that perform hypothesis tests (e.g., the R function t.test)"
    ), status = list(enum = "wrong"))
  ))
})

test_that("testHtest reports an error in generated as a runtime error", {
  judge <- local_judge_with()
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, testHtest("test", function(env) stop("boom"), expected))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = "test"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testHtest reports a warning in generated", {
  judge <- local_judge_with(test = new_htest())
  judge$expected <- new_htest()

  feedback <- capture_feedback(judge, {
    testHtest("test", function(env) {
      warning("careful")
      env$test
    }, expected)
  })

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "correct"))
  expect_equal(feedback, list(
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "start-test", expected = "\np-value = 0.04\nconfidence interval = 1, 2", description = "test"),
    list(command = "close-test", generated = "\np-value = 0.04\nconfidence interval = 1, 2", status = list(enum = "correct"))
  ))
})
