context({
  testcase("Numbers", {
    testEqual("x", function(studentEnv) studentEnv$x, 42)
    testEqual("approx", function(studentEnv) studentEnv$approx, 3.14, tolerance = 0.01)
    testIdentical("count", function(studentEnv) studentEnv$count, 3L)
  })
  testcase("Text", {
    testEqual("name", function(studentEnv) studentEnv$name, "dodona",
              comparator = function(generated, expected, ...) identical(tolower(generated), expected),
              formatter = toupper)
    testEqual("symbol", function(studentEnv) studentEnv$symbol, "ℹ")
  })
  testcase("Comparator feedback", {
    testEqual("x with a warning", function(studentEnv) studentEnv$x, 42,
              comparator = function(generated, expected, ...) {
                warning("comparing anyway")
                message("ℹ comparing")
                isTRUE(all.equal(generated, expected))
              })
    testEqual("x with a hint", function(studentEnv) studentEnv$x, 42,
              comparator = function(generated, expected, ...) {
                get_reporter()$add_message("Compared **x** with `42`", type = "markdown")
                isTRUE(all.equal(generated, expected))
              })
  })
  testcaseAssert("x exists", function(studentEnv) isTRUE(exists("x", studentEnv)))
}, preExec = {
  given <- 10
})

context({
  testcase("Seeded", {
    testEqual("noise", function(studentEnv) studentEnv$noise, 0.2655087, tolerance = 1e-6)
    testEqual("doubled", function(studentEnv) studentEnv$doubled, 20)
  })
}, preExec = {
  set.seed(1)
  given <- 10
})

context({
  testcase("Last value", {
    testEqual("evaluationResult", function(studentEnv) studentEnv$evaluationResult, 42)
  })
}, preExec = {
  given <- 10
})
