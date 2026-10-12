expected <- data.frame(id = 1:8,
                       name = c("ada", "bob", "cleo", "dan", "eve", "fay", "gus", "hal"),
                       score = c(9.5, 7, 8.25, 6, 10, 5.5, 7.75, 8))

context({
  testcase("Any order", {
    testDF("scores", function(studentEnv) studentEnv$scores, expected)
  })
  testcase("Strict order", {
    testDF("ordered_scores", function(studentEnv) studentEnv$ordered_scores, expected,
           ignore_row_order = FALSE, ignore_col_order = FALSE)
  })
  testcase("Comparator", {
    testDF("row count", function(studentEnv) studentEnv$scores, expected,
           comparator = function(generated, expected, ...) isTRUE(nrow(generated) == nrow(expected)))
  })
})
