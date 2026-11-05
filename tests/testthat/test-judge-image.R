# Kept source is loaded with every function, so every submission carries it in memory.
test_that("the judge image's packages keep no source", {
  skip_if_not(in_judge_image, "only the judge image's library is checked")

  packages <- c("jsonlite", "base64enc", "rlang", "purrr", "knitr", "ggplot2", "dplyr")
  for (package in packages) {
    ns <- asNamespace(package)
    # Functions copied in from other packages keep source by their own choice.
    functions <- Filter(
      function(f) is.function(f) && identical(environment(f), ns),
      as.list(ns, all.names = TRUE)
    )
    kept <- names(Filter(function(f) !is.null(attr(f, "srcref")), functions))

    expect(
      length(kept) == 0,
      sprintf(
        "%s keeps source on %d functions, e.g. %s",
        package, length(kept), paste(head(kept, 5), collapse = ", ")
      )
    )
  }
})
