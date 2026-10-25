context({
  testcaseAssert("purrr is not loaded before it is used", function(studentEnv) !isNamespaceLoaded("purrr"))
})

squares <- map_dbl(1:3, function(x) x^2)

context({
  testcase("purrr without library()", {
    testEqual("squares", function(studentEnv) studentEnv$squares, squares)
  })
})
