context({
  testcase("Broken formatter", {
    testEqual("x", function(studentEnv) studentEnv$x, 1, formatter = function(value) stop("bad formatter"))
  })
  testcase("Still runs", {
    testEqual("x", function(studentEnv) studentEnv$x, 1)
  })
})
