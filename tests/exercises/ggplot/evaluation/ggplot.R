library(ggplot2)

expected_p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl))) + facet_wrap(~am)
expected_labelled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "Miles per gallon")
expected_scaled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10()

context({
  testcase("Scatter plot", {
    testGGPlot("p", function(studentEnv) studentEnv$p, expected_p)
  })
  testcase("Labels", {
    testGGPlot("p_labelled", function(studentEnv) studentEnv$p_labelled, expected_labelled,
               show_expected = FALSE, test_label = TRUE)
  })
  testcase("Scale", {
    testGGPlot("p_scaled", function(studentEnv) studentEnv$p_scaled, expected_scaled,
               show_expected = FALSE, test_scale = TRUE)
  })
  testcase("Facet type", {
    testGGPlot("p_faceted", function(studentEnv) studentEnv$p_faceted, expected_p,
               show_expected = FALSE, ignore_facet_type = FALSE)
  })
})
