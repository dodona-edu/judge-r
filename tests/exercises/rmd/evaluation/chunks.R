contextWithRmd({
  testcase("Chunks", {
    testEqual("total", function(studentEnv) studentEnv$total, 6)
    testEqual("evaluationResult", function(studentEnv) studentEnv$evaluationResult, "done")
  })
})
