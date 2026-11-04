context({
  testcase("First context", {
    testEqual("x", function(studentEnv) studentEnv$x, median(c(40, 42, 44)))
  })
})

context({
  testcase("Second context", {
    testEqual("x", function(studentEnv) studentEnv$x, median(c(40, 42, 44)))
  })
})
