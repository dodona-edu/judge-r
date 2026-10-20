withr::local_package("ggplot2")

test_that("get_type turns a ggproto class into the name of its constructor", {
  get_type <- local_judge()$get_type
  plot <- ggplot(mtcars, aes(mpg, wt)) + geom_point()

  expect_equal(get_type(plot$layers[[1]]$geom), "geom_point")
  expect_equal(get_type(plot$layers[[1]]$stat), "stat_identity")
  expect_equal(get_type(plot$facet), "facet_null")
})

test_that("get_params combines the geom, stat and aesthetic parameters of a layer", {
  get_params <- local_judge()$get_params
  layer <- (ggplot(mtcars, aes(mpg)) + geom_histogram(bins = 10, fill = "blue"))$layers[[1]]

  params <- get_params(layer)

  expect_equal(params$linejoin, "mitre")
  expect_equal(params$bins, 10)
  expect_equal(params$fill, "blue")
})

test_that("get_params prefers aesthetic over stat over geom parameters", {
  get_params <- local_judge()$get_params
  layer <- list(
    geom_params = list(a = "geom", b = "geom", c = "geom"),
    stat_params = list(b = "stat", c = "stat"),
    aes_params = list(c = "aes")
  )

  expect_equal(get_params(layer), list(a = "geom", b = "stat", c = "aes"))
})

test_that("test_data_layer accepts the same data", {
  test_data_layer <- local_judge()$test_data_layer

  expect_true(test_data_layer(mtcars, mtcars)$equal)
})

test_that("test_data_layer accepts extra columns in another order", {
  test_data_layer <- local_judge()$test_data_layer

  expect_true(test_data_layer(data.frame(a = 1, b = 2), data.frame(c = 3, b = 2, a = 1))$equal)
})

test_that("test_data_layer rejects other values in the solution's columns", {
  test_data_layer <- local_judge()$test_data_layer

  result <- test_data_layer(data.frame(a = 1:2, b = 3:4), data.frame(a = 1:2, b = 4:3))

  expect_false(result$equal)
  expect_equal(result$feedback, "The data you used in your ggplot contains the right column names but is wrong")
})

test_that("test_data_layer names the column that is missing", {
  test_data_layer <- local_judge()$test_data_layer

  result <- test_data_layer(data.frame(a = 1, b = 2), data.frame(a = 1))

  expect_false(result$equal)
  expect_equal(result$feedback, "You did not add the correct data, missing column named: b")
})

test_that("test_data_layer names every missing column in one sentence", {
  test_data_layer <- local_judge()$test_data_layer

  result <- test_data_layer(data.frame(a = 1, b = 2, c = 3), data.frame(a = 1))

  expect_false(result$equal)
  # Bug #98: testGGPlot repeats the missing data message for every missing column
  expect_failure(expect_length(result$feedback, 1))
})

test_that("test_data_layer uses the singular for one missing column", {
  test_data_layer <- local_judge()$test_data_layer

  result <- test_data_layer(data.frame(a = 1, b = 2, c = 3), data.frame(a = 1, b = 2))

  expect_false(result$equal)
  # Bug #103: testGGPlot's missing data message picks singular or plural from the columns that are present
  expect_failure(expect_equal(result$feedback, "You did not add the correct data, missing column named: c"))
})

test_that("test_data_layer handles a solution without plot data", {
  test_data_layer <- local_judge()$test_data_layer
  plot <- ggplot() + geom_point(data = mtcars, aes(mpg, wt))

  # Bug #60: testGGPlot testing data for ggplots without data parameter fails
  expect_failure(expect_no_error(test_data_layer(plot$data, plot$data)))
})

test_that("test_facet_layer accepts any facets when the solution has none", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer(ggplot(mtcars)$facet, (ggplot(mtcars) + facet_wrap(~cyl))$facet)

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_facet_layer rejects a plot without facets", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer((ggplot(mtcars) + facet_wrap(~cyl))$facet, ggplot(mtcars)$facet)

  expect_equal(result, list(equal = FALSE, feedback = "Did you define a multipanel plot?"))
})

test_that("test_facet_layer accepts a facet_grid for a facet_wrap on the same variables", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer((ggplot(mtcars) + facet_wrap(~cyl))$facet, (ggplot(mtcars) + facet_grid(cyl ~ .))$facet)

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_facet_layer lists the solution's variables when the variables differ", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer((ggplot(mtcars) + facet_grid(cyl ~ gear))$facet, (ggplot(mtcars) + facet_wrap(~cyl))$facet)

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Did you make a multipanel plot with rows or columns at least divided by (cyl, gear)?"
  ))
})

test_that("test_facet_layer compares the variables of two facet_wraps when the type counts", {
  test_facet_layer <- local_judge()$test_facet_layer
  solution <- (ggplot(mtcars) + facet_wrap(~cyl))$facet

  same <- test_facet_layer(solution, (ggplot(mtcars) + facet_wrap(~cyl))$facet, ignore_facet_type = FALSE)
  other <- test_facet_layer(solution, (ggplot(mtcars) + facet_wrap(~gear))$facet, ignore_facet_type = FALSE)

  expect_equal(same, list(equal = TRUE, feedback = ""))
  expect_equal(other, list(
    equal = FALSE,
    feedback = "Did you make a multipanel plot with rows or columns at least divided by (cyl)?"
  ))
})

test_that("test_facet_layer asks for a facet_wrap when the type counts", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer(
    (ggplot(mtcars) + facet_wrap(~cyl))$facet,
    (ggplot(mtcars) + facet_grid(cyl ~ .))$facet,
    ignore_facet_type = FALSE
  )

  expect_equal(result, list(equal = FALSE, feedback = "Did you use the facet_wrap layer?"))
})

test_that("test_facet_layer asks for a facet_grid when the type counts", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer(
    (ggplot(mtcars) + facet_grid(cyl ~ .))$facet,
    (ggplot(mtcars) + facet_wrap(~cyl))$facet,
    ignore_facet_type = FALSE
  )

  expect_equal(result, list(equal = FALSE, feedback = "Did you use the facet_grid layer?"))
})

test_that("test_facet_layer accepts a facet_grid with rows and columns swapped when the type counts", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer(
    (ggplot(mtcars) + facet_grid(cyl ~ gear))$facet,
    (ggplot(mtcars) + facet_grid(gear ~ cyl))$facet,
    ignore_facet_type = FALSE
  )

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_facet_layer rejects a facet_grid on other variables when the type counts", {
  test_facet_layer <- local_judge()$test_facet_layer

  result <- test_facet_layer(
    (ggplot(mtcars) + facet_grid(cyl ~ gear))$facet,
    (ggplot(mtcars) + facet_grid(cyl ~ am))$facet,
    ignore_facet_type = FALSE
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Did you make a multipanel plot with rows or columns at least divided by (cyl, gear)?"
  ))
})

test_that("test_geom_layer accepts anything when the solution has no layers", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(ggplot(mtcars, aes(mpg, wt)), ggplot(mtcars, aes(mpg, wt)) + geom_line())

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer accepts the same layer", {
  test_geom_layer <- local_judge()$test_geom_layer
  plot <- ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "red")

  expect_equal(test_geom_layer(plot, plot), list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer accepts extra layers", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(),
    ggplot(mtcars, aes(mpg, wt)) + geom_line() + geom_point()
  )

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer asks for a missing geom", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(ggplot(mtcars, aes(mpg, wt)) + geom_point(), ggplot(mtcars, aes(mpg, wt)) + geom_line())

  expect_equal(result, list(equal = FALSE, feedback = "Have you added a `geom_point` layer with a `+` operator?"))
})

test_that("test_geom_layer needs a layer for every layer in the solution", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(size = 1) + geom_point(size = 2),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(size = 1)
  )

  expect_equal(result, list(equal = FALSE, feedback = "Have you added a `geom_point` layer with a `+` operator?"))
})

test_that("test_geom_layer accepts aesthetics from the plot or from the layer", {
  test_geom_layer <- local_judge()$test_geom_layer
  in_plot <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  in_layer <- ggplot(mtcars) + geom_point(aes(mpg, wt))

  expect_true(test_geom_layer(in_plot, in_layer)$equal)
  expect_true(test_geom_layer(in_layer, in_plot)$equal)
})

test_that("test_geom_layer asks for a missing aesthetic", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(ggplot(mtcars) + geom_point(aes(mpg, wt)), ggplot(mtcars, aes(mpg)) + geom_point())

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Have you specified the `y` aesthetic needed in the `geom_point` layer?"
  ))
})

test_that("test_geom_layer names the expected value of a wrong aesthetic", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = cyl)),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = gear))
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Found `~gear` for the `colour` aesthetic. Expected `~cyl`."
  ))
})

test_that("test_geom_layer looks past a layer of the same geom with other aesthetics", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = cyl)),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = gear)) + geom_point(aes(colour = cyl))
  )

  expect_true(result$equal)
})

test_that("test_geom_layer still rejects when no layer of the geom has the right aesthetics", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = cyl)),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(aes(colour = gear)) + geom_point(aes(colour = disp))
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Found `~gear` for the `colour` aesthetic. Expected `~cyl`."
  ))
})

test_that("test_geom_layer asks for a missing parameter", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "red"),
    ggplot(mtcars, aes(mpg, wt)) + geom_point()
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Have you added a `geom_point` layer with the correct parameters?"
  ))
})

test_that("test_geom_layer asks for the right value of a parameter", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "red"),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "blue")
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Have you added a `geom_point` layer with the correct parameters?"
  ))
})

test_that("test_geom_layer accepts extra parameters", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "red"),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(colour = "red", size = 3)
  )

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer looks past a layer of the same geom with other parameters", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(mpg, wt)) + geom_point(size = 2),
    ggplot(mtcars, aes(mpg, wt)) + geom_point(size = 1) + geom_point(size = 2)
  )

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer accepts a stat layer for the equivalent geom layer", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(cars, aes(speed, dist)) + geom_smooth(method = "loess", formula = y ~ x),
    ggplot(cars, aes(speed, dist)) + stat_smooth(method = "loess", formula = y ~ x)
  )

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer accepts the equivalent geom layer for a stat layer", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::stat_smooth(method = "loess", formula = y ~ x),
    ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::geom_smooth(method = "loess", formula = y ~ x)
  )

  expect_equal(result, list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer accepts stat_count and geom_bar for each other", {
  test_geom_layer <- local_judge()$test_geom_layer
  geom <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::geom_bar()
  stat <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl))) + ggplot2::stat_count()

  expect_true(test_geom_layer(geom, stat)$equal)
  expect_true(test_geom_layer(stat, geom)$equal)
})

test_that("test_geom_layer accepts stat_boxplot and geom_boxplot for each other", {
  test_geom_layer <- local_judge()$test_geom_layer
  geom <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::geom_boxplot()
  stat <- ggplot2::ggplot(mtcars, ggplot2::aes(factor(cyl), mpg)) + ggplot2::stat_boxplot()

  expect_true(test_geom_layer(geom, stat)$equal)
  expect_true(test_geom_layer(stat, geom)$equal)
})

test_that("test_geom_layer rejects a stat layer with other parameters than the geom layer", {
  test_geom_layer <- local_judge()$test_geom_layer
  geom <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::geom_smooth(method = "lm", formula = y ~ x)
  stat <- ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::stat_smooth(method = "loess", formula = y ~ x)

  expect_false(test_geom_layer(geom, stat)$equal)
  expect_false(test_geom_layer(stat, geom)$equal)
})

test_that("test_geom_layer compares a parameter the geom layer leaves out with its default", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::stat_smooth(method = "loess", formula = y ~ x, n = 20),
    ggplot2::ggplot(cars, ggplot2::aes(speed, dist)) + ggplot2::geom_smooth(method = "loess", formula = y ~ x)
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Have you added a `geom_smooth` layer with the correct parameters?"
  ))
})

test_that("ggplot2 layers still record the call that created them", {
  expect(
    !is.null(ggplot2::geom_point()$constructor),
    "ggplot2 layers have no `constructor` field anymore: get_default_params in utils-plot.R needs it"
  )
})

test_that("test_geom_layer accepts the same geom_pointrange layer", {
  test_geom_layer <- local_judge()$test_geom_layer
  plot <- ggplot(mtcars, aes(cyl, mpg, ymin = mpg - 1, ymax = mpg + 1)) + geom_pointrange()

  expect_equal(test_geom_layer(plot, plot), list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer accepts the same geom_crossbar layer", {
  test_geom_layer <- local_judge()$test_geom_layer
  plot <- ggplot(mtcars, aes(factor(cyl), mpg, ymin = mpg - 1, ymax = mpg + 1)) + geom_crossbar()

  expect_equal(test_geom_layer(plot, plot), list(equal = TRUE, feedback = ""))
})

test_that("test_geom_layer rejects a geom_pointrange layer with other aesthetics", {
  test_geom_layer <- local_judge()$test_geom_layer

  result <- test_geom_layer(
    ggplot(mtcars, aes(cyl, mpg, ymin = mpg - 1, ymax = mpg + 1)) + geom_pointrange(),
    ggplot(mtcars, aes(cyl, mpg, ymin = mpg - 2, ymax = mpg + 1)) + geom_pointrange()
  )

  expect_false(result$equal)
})

test_that("test_geom_layer rejects a geom_pointrange layer with another parameter", {
  test_geom_layer <- local_judge()$test_geom_layer
  aesthetics <- aes(cyl, mpg, ymin = mpg - 1, ymax = mpg + 1)

  result <- test_geom_layer(
    ggplot(mtcars, aesthetics) + geom_pointrange(colour = "red"),
    ggplot(mtcars, aesthetics) + geom_pointrange(colour = "blue")
  )

  expect_equal(result, list(
    equal = FALSE,
    feedback = "Have you added a `geom_pointrange` layer with the correct parameters?"
  ))
})
