context({
  testcase("square", {
    testEqual("square(3)", function(studentEnv) studentEnv$square(3), 9)
    testEqual("square(-2)", function(studentEnv) studentEnv$square(-2), 4)
  })
  testcaseAssert("greet answers", function(studentEnv) nchar(studentEnv$greet("Ada")) > 0)
  testcase("greet", {
    testEqual("greet(\"Ada\")", function(studentEnv) studentEnv$greet("Ada"), "Hello, Ada!")
  })
}, preExec = {
  given <- 10
})
