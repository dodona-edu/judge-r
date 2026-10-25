evaluation_level <- compiler::enableJIT(-1)

context({
  testcase("JIT level", {
    testEqual("student code", function(studentEnv) studentEnv$level, 3)
    testEqual("student function called by a test", function(studentEnv) studentEnv$current_level(), 3)
    testEqual("evaluation code", function(studentEnv) evaluation_level, 0)
  })
})
