test_that("expr_type calls syntactic literals constants", {
  expr_type <- local_judge()$expr_type

  for (literal in list(1, 1L, "a", TRUE, NA, NULL)) {
    expect_equal(expr_type(literal), "constant")
  }
})

test_that("expr_type names symbols, calls, pairlists and expressions", {
  expr_type <- local_judge()$expr_type

  expect_equal(expr_type(quote(x)), "symbol")
  expect_equal(expr_type(quote(f(x))), "call")
  expect_equal(expr_type(quote(function(x) x)[[2]]), "pairlist")
  expect_equal(expr_type(expression(1)), "expression")
})

test_that("expr_type falls back to typeof for anything else", {
  expr_type <- local_judge()$expr_type

  expect_equal(expr_type(c(1, 2)), "double")
  expect_equal(expr_type(list(1)), "list")
})

test_that("get_node follows a path of child indices", {
  get_node <- local_judge()$get_node
  code <- parse(text = "a <- b <- mean(1)")

  expect_equal(get_node(c(1, 3), code), quote(b <- mean(1)))
  expect_equal(get_node(c(1, 3, 3), code), quote(mean(1)))
})

test_that("get_node returns the code itself for an empty path", {
  get_node <- local_judge()$get_node
  code <- parse(text = "a <- 1")

  expect_identical(get_node(NULL, code), code)
})

test_that("concat_lists returns the other list when one is NULL", {
  concat_lists <- local_judge()$concat_lists

  expect_equal(concat_lists(NULL, list(a = list(1))), list(a = list(1)))
  expect_equal(concat_lists(list(a = list(1)), NULL), list(a = list(1)))
})

test_that("concat_lists keeps the keys of both lists", {
  concat_lists <- local_judge()$concat_lists

  expect_equal(concat_lists(list(a = list(1)), list(b = list(2))), list(a = list(1), b = list(2)))
})

test_that("concat_lists appends the values of a shared key without duplicates", {
  concat_lists <- local_judge()$concat_lists

  expect_equal(concat_lists(list(a = list(1, 2)), list(a = list(2, 3))), list(a = list(1, 2, 3)))
})

test_that("find_assign records the path of assignments with <- and =", {
  find_assign <- local_judge()$find_assign
  code <- parse(text = c("a <- 1", "b = 2"))

  expect_equal(find_assign(code), list(a = list(1L), b = list(2L)))
})

test_that("find_assign records every assignment to the same variable", {
  find_assign <- local_judge()$find_assign
  code <- parse(text = c("x <- 1", "x <- 2"))

  expect_equal(find_assign(code), list(x = list(1L, 2L)))
})

test_that("find_assign records chained assignments", {
  find_assign <- local_judge()$find_assign
  code <- parse(text = "a <- b <- mean(1)")

  expect_equal(find_assign(code), list(a = list(1L), b = list(c(1L, 3L))))
})

test_that("find_assign records assignments inside a function body", {
  find_assign <- local_judge()$find_assign
  code <- parse(text = "f <- function(x) { y <- x }")

  expect_equal(find_assign(code), list(f = list(1L), y = list(c(1L, 3L, 3L, 2L))))
})

test_that("find_assign gives an empty path for a single assignment call", {
  find_assign <- local_judge()$find_assign

  expect_equal(find_assign(quote(a <- 1)), list(a = list(NULL)))
})

test_that("find_assign ignores named arguments", {
  find_assign <- local_judge()$find_assign

  expect_null(find_assign(parse(text = "f(a = 1)")))
})

test_that("find_assign returns NULL for code without assignments", {
  find_assign <- local_judge()$find_assign

  expect_null(find_assign(parse(text = "mean(1)")))
  expect_null(find_assign(quote(x)))
  expect_null(find_assign(1))
})

test_that("find_assign rejects values that are not code", {
  find_assign <- local_judge()$find_assign

  expect_error(find_assign(list(1)), "Don't know how to handle type list")
})

test_that("is_function_used finds a call anywhere in the code", {
  judge <- local_judge()
  code <- parse(text = c("values <- c(1, 2)", "round(mean(values))"))

  expect_true(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used finds a call on the right of an assignment", {
  judge <- local_judge()
  code <- parse(text = "average <- mean(1)")

  expect_true(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used finds a call made through the native pipe", {
  judge <- local_judge()
  code <- parse(text = "average <- 1 |> mean()")

  expect_true(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used finds a namespaced call", {
  judge <- local_judge()
  code <- parse(text = "base::mean(1)")

  expect_true(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used finds a call in the body of a function without arguments", {
  judge <- local_judge()
  code <- parse(text = "f <- function() mean(1)")

  expect_true(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used returns FALSE when the function is not called", {
  judge <- local_judge()
  code <- parse(text = c("values <- c(1, 2)", "average <- sum(values) / length(values)"))

  expect_false(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used skips empty arguments", {
  judge <- local_judge()
  code <- parse(text = "x[1, ]")

  expect_false(judge$is_function_used("mean", judge$find_assign(code), code, code))
})

test_that("is_function_used follows a symbol to the code assigned to it", {
  judge <- local_judge()
  code <- parse(text = c("x <- mean(1)", "y <- 2"))
  paths <- judge$find_assign(code)

  expect_true(judge$is_function_used("mean", paths, quote(x), code))
  expect_false(judge$is_function_used("mean", paths, quote(y), code))
})

test_that("is_function_used stops following variables that are assigned to each other", {
  judge <- local_judge()
  code <- parse(text = c("a <- b", "b <- a"))

  expect_false(judge$is_function_used("mean", judge$find_assign(code), quote(a), code))
})

test_that("is_function_used rejects values that are not code", {
  is_function_used <- local_judge()$is_function_used

  expect_error(is_function_used("mean", NULL, function() 1, NULL), "Don't know how to handle type closure")
})

test_that("is_function_used handles code that defines a function with arguments", {
  judge <- local_judge()
  code <- parse(text = c("f <- function(x) x", "average <- mean(1)"))

  # Bug #102: testFunctionUsed and testFunctionUsedInVar fail on code that defines a function with arguments
  expect_failure(expect_no_error(judge$is_function_used("mean", judge$find_assign(code), code, code)))
})

test_that("is_function_used does not warn about passing calls and expressions to purrr", {
  judge <- local_judge()
  code <- parse(text = "average <- mean(1)")

  # Lifecycle warns once per session; a judge run is a fresh session, a test run is not.
  withr::local_options(lifecycle_verbosity = "warning")

  # purrr 1.2 deprecates calls and expressions in some(): #101
  expect_no_warning(
    judge$is_function_used("mean", judge$find_assign(code), code, code),
    class = "lifecycle_warning_deprecated"
  )
})

test_that("is_function_used_in_var finds a call in the assignment to the variable", {
  judge <- local_judge()
  code <- parse(text = "total <- sum(1)")

  expect_true(judge$is_function_used_in_var("sum", judge$find_assign(code), "total", code))
})

test_that("is_function_used_in_var ignores calls in assignments to other variables", {
  judge <- local_judge()
  code <- parse(text = c("other <- sum(1)", "total <- 1"))

  expect_false(judge$is_function_used_in_var("sum", judge$find_assign(code), "total", code))
})

test_that("is_function_used_in_var checks every assignment to the variable", {
  judge <- local_judge()
  code <- parse(text = c("total <- 1", "total <- sum(1)"))

  expect_true(judge$is_function_used_in_var("sum", judge$find_assign(code), "total", code))
})

test_that("is_function_used_in_var follows chained assignments", {
  judge <- local_judge()
  code <- parse(text = "a <- b <- mean(1)")
  paths <- judge$find_assign(code)

  expect_true(judge$is_function_used_in_var("mean", paths, "a", code))
  expect_true(judge$is_function_used_in_var("mean", paths, "b", code))
})

test_that("is_function_used_in_var follows the variables used in the assignment", {
  judge <- local_judge()
  code <- parse(text = c("subtotal <- sum(1)", "total <- grand <- subtotal"))

  expect_true(judge$is_function_used_in_var("sum", judge$find_assign(code), "total", code))
})

test_that("is_function_used_in_var follows variables that are assigned to each other", {
  judge <- local_judge()
  code <- parse(text = c("a <- b", "b <- a"))

  expect_false(judge$is_function_used_in_var("mean", judge$find_assign(code), "a", code))
})

test_that("is_function_used_in_var finds a call in a function assigned to the variable", {
  judge <- local_judge()
  code <- parse(text = "f <- function(x) mean(x)")

  # Bug #102: testFunctionUsed and testFunctionUsedInVar fail on code that defines a function with arguments
  expect_failure(expect_no_error(judge$is_function_used_in_var("mean", judge$find_assign(code), "f", code)))
})

test_that("is_function_used_in_var counts an assignment to an element with $<-", {
  judge <- local_judge()
  code <- parse(text = c("l <- list()", "l$a <- mean(1)"))

  # Bug #63: `[[<-` and `$<-` are not picked up in `testFunctionUsed` and `testFunctionUsedInVar`
  expect_failure(expect_true(judge$is_function_used_in_var("mean", judge$find_assign(code), "l", code)))
})

test_that("is_function_used_in_var counts an assignment to an element with [[<-", {
  judge <- local_judge()
  code <- parse(text = c("l <- list()", "l[['a']] <- mean(1)"))

  # Bug #63: `[[<-` and `$<-` are not picked up in `testFunctionUsed` and `testFunctionUsedInVar`
  expect_failure(expect_true(judge$is_function_used_in_var("mean", judge$find_assign(code), "l", code)))
})
