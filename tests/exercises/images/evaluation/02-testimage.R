context({
  testcase("Required image", {
    testImage(function(studentEnv) studentEnv$draw())
  })
  testcase("Optional image", {
    testImage(function(studentEnv) studentEnv$draw(), failIfAbsent = FALSE, width = 400, height = 300)
  })
  testcase("Failing image", {
    testImage(function(studentEnv) studentEnv$draw_broken())
  })
})
