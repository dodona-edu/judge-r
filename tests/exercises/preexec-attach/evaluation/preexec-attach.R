context({
  testcase("Data attached in preExec", {
    testEqual("passed", function(studentEnv) studentEnv$passed, exam_score >= 10)
  })
  testcase("Package attached in preExec", {
    testEqual("chance", function(studentEnv) studentEnv$chance, inv.logit(0))
  })
}, preExec = {
  exam <- data.frame(exam_score = c(8, 12, 15))
  attach(exam)
  library(boot)
})
