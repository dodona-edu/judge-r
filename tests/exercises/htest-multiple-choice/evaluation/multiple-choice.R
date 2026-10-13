options <- c("mean", "median", "mode", "range")

context({
  testcase("Verified", {
    testMultipleChoice("best measure", function(studentEnv) studentEnv$choice, c("mean", "median"),
                       possible_answers = options, verify_answer = TRUE,
                       feedback = list(mode = "the mode is not a measure of centre here"))
  })
  testcase("Shown answer", {
    testMultipleChoice("best measure again", function(studentEnv) studentEnv$choice, c("mean", "median"),
                       possible_answers = options, verify_answer = TRUE, show_expected = TRUE)
  })
  testcase("Not verified", {
    testMultipleChoice("favourite", function(studentEnv) studentEnv$favourite, "mean",
                       possible_answers = options)
  })
})
