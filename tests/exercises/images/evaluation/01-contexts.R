contextWithImage({
  testcase("Required image", {
    testEqual("n", function(studentEnv) studentEnv$n, 3)
  })
})

contextWithImage({
  testcase("Optional image", {
    testEqual("n", function(studentEnv) studentEnv$n, 3)
  })
}, failIfAbsent = FALSE, width = 400, height = 300)
