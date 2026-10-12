library(ggplot2)
secret <- "only for the tests"

context({
  testcase("Package attached by the student", {
    testEqual("titled", function(studentEnv) studentEnv$titled, "Hello World")
  })
  testcaseAssert("student package is not attached for the tests",
                 function(studentEnv) !("package:tools" %in% search()))
  testcase("Test code is hidden from the student", {
    testEqual("hidden", function(studentEnv) studentEnv$hidden, c(FALSE, FALSE, FALSE, FALSE))
  })
})
