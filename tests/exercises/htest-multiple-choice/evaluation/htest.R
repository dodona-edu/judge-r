expected_test <- t.test(c(5.1, 4.9, 5.6, 5.8, 6.0, 5.3), mu = 5)

context({
  testcase("t-test", {
    testHtest("test", function(studentEnv) studentEnv$test, expected_test)
  })
  testcase("t-test details", {
    testHtest("test", function(studentEnv) studentEnv$test, expected_test,
              test_p_value = FALSE, test_interval = FALSE, test_statistic = TRUE,
              test_alternative = TRUE, test_confidence_level = TRUE, test_method = TRUE)
  })
})
