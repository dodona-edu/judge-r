withr::local_package("ggplot2")

plot_message <- "<img style=\"max-width:100%; width:450px;\" src=\"data:image/png;base64,<png>\"/>"

test_that("testGGPlot accepts the same plot and shows it", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point())
  })

  expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "close-test", generated = "", status = list(enum = "correct"))
  ))
})

test_that("testGGPlot shows the expected plot next to a wrong one", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_line())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point())
  })

  expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "append-message", message = list(description = "Have you added a `geom_point` layer with a `+` operator?", format = "plain")),
    list(command = "close-test", generated = "", status = list(enum = "wrong"))
  ))
})

test_that("testGGPlot hides the expected plot with show_expected = FALSE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_line())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point(), show_expected = FALSE)
  })

  expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "append-message", message = list(description = "Have you added a `geom_point` layer with a `+` operator?", format = "plain")),
    list(command = "close-test", generated = "", status = list(enum = "wrong"))
  ))
})

test_that("testGGPlot rejects a value that isn't a ggplot without showing the expected plot", {
  judge <- local_judge_with(p = 1)

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point())
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = "You did not generate a ggplot", format = "plain")),
    list(command = "close-test", generated = "", status = list(enum = "wrong"))
  ))
})

test_that("testGGPlot compares the plot data", {
  judge <- local_judge_with(p = ggplot(mtcars[1:5, ], aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point(), show_expected = FALSE)
  })

  expect_equal(feedback[[3]], list(command = "append-message", message = list(
    description = "The data you used in your ggplot contains the right column names but is wrong",
    format = "plain"
  )))
  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot ignores the plot data with test_data = FALSE", {
  judge <- local_judge_with(p = ggplot(mtcars[1:5, ], aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point(), test_data = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot ignores the geom layers with test_geom = FALSE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_line())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point(), test_geom = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot compares the facets", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~am),
               show_expected = FALSE)
  })

  expect_equal(feedback[[3]], list(command = "append-message", message = list(
    description = "Did you define a multipanel plot?",
    format = "plain"
  )))
  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot ignores the facets with test_facet = FALSE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~am),
               test_facet = FALSE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot accepts another facet type on the same variables by default", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ .))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~am))
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot checks the facet type with ignore_facet_type = FALSE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(am ~ .))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~am),
               show_expected = FALSE, ignore_facet_type = FALSE)
  })

  expect_equal(feedback[[3]], list(command = "append-message", message = list(
    description = "Did you use the facet_wrap layer?",
    format = "plain"
  )))
  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot ignores the labels by default", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "MPG"))
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot compares the axis labels with test_label = TRUE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight"))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "MPG"),
               show_expected = FALSE, test_label = TRUE)
  })

  expect_equal(feedback[[3]], list(command = "append-message", message = list(
    description = "Did you specify the correct labels?",
    format = "plain"
  )))
  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot compares only the axis labels with test_label = TRUE", {
  judge <- local_judge_with(
    p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "MPG", title = "Cars")
  )

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "MPG"),
               test_label = TRUE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot ignores the scales by default", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10())
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot compares the scales with test_scale = TRUE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10(),
               show_expected = FALSE, test_scale = TRUE)
  })

  expect_equal(feedback[[3]], list(command = "append-message", message = list(
    description = "Did you specify the correct scales?",
    format = "plain"
  )))
  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot accepts a scale named after its default axis title with test_scale = TRUE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10("wt"))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10(),
               show_expected = FALSE, test_scale = TRUE)
  })

  expect_equal(feedback[[3]]$status, list(enum = "correct"))
})

test_that("testGGPlot rejects a scale with another axis title with test_scale = TRUE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10("weight"))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10(),
               show_expected = FALSE, test_scale = TRUE)
  })

  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot rejects a scale with other limits with test_scale = TRUE", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10(limits = c(1, 10)))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10(),
               show_expected = FALSE, test_scale = TRUE)
  })

  expect_equal(feedback[[4]]$status, list(enum = "wrong"))
})

test_that("testGGPlot reports only the first check that fails", {
  judge <- local_judge_with(p = ggplot(mtcars[1:5, ], aes(wt, mpg)) + geom_line())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point(), show_expected = FALSE)
  })

  expect_equal(feedback[[3]]$message$description, "The data you used in your ggplot contains the right column names but is wrong")
  expect_length(feedback, 4)
})

test_that("testGGPlot removes the images it saves", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_line())
  before <- tempdir_pngs()

  capture_feedback(judge, testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point()))

  expect_equal(tempdir_pngs(), before)
})

test_that("testGGPlot removes the images it saves when a check fails with an error", {
  local_tempdir_pngs()
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())
  before <- tempdir_pngs()

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point(), ignore_facet_types = FALSE)
  })

  expect_equal(tail(feedback, 1)[[1]], list(
    command = "close-test",
    generated = "unused argument (ignore_facet_types = FALSE)",
    status = list(enum = "runtime error")
  ))
  # Bug #120: testImage and testGGPlot leave PNG files in the temporary directory
  expect_failure(expect_equal(tempdir_pngs(), before))
})

test_that("testGGPlot reports an error in generated as a runtime error", {
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) stop("boom"), ggplot(mtcars, aes(wt, mpg)) + geom_point())
  })

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "close-test", generated = "", status = list(enum = "wrong")),
    list(command = "start-test", expected = "", description = "p"),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error"))
  ))
})

test_that("testGGPlot reports an error drawing the student's plot as a runtime error", {
  local_tempdir_pngs()
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, unknown)) + geom_point())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point())
  })

  expect_length(feedback, 4)
  expect_equal(feedback[[2]], list(command = "close-test", generated = "", status = list(enum = "wrong")))
  expect_match(feedback[[4]]$generated, "object 'unknown' not found")
  expect_equal(feedback[[4]]$status, list(enum = "runtime error"))
})

test_that("testGGPlot shows no deprecation warning", {
  # Lifecycle warns once per session; a judge run is a fresh session, a test run is not.
  withr::local_options(lifecycle_verbosity = "warning")
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- capture_feedback(judge, testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(wt, mpg)) + geom_point()))

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "correct"))
  expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "close-test", generated = "", status = list(enum = "correct"))
  ))
})

test_that("testGGPlot accepts the same plot when the data is given to a layer", {
  local_tempdir_pngs()
  judge <- local_judge_with(p = ggplot() + geom_point(aes(wt, mpg), data = mtcars))

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot() + geom_point(aes(wt, mpg), data = mtcars))
  })

  expect_equal(feedback[[1]], list(command = "start-test", expected = "", description = "p"))
  # Bug #60: testGGPlot testing data for ggplots without data parameter fails
  expect_failure(expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "close-test", generated = "", status = list(enum = "correct"))
  )))
})

test_that("testGGPlot reports a warning in generated", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(wt, mpg)) + geom_point())

  feedback <- suppressWarnings(capture_feedback(judge, {
    testGGPlot("p", function(env) {
      warning("careful")
      env$p
    }, ggplot(mtcars, aes(wt, mpg)) + geom_point())
  }))

  expect_equal(tail(feedback, 1)[[1]]$status, list(enum = "correct"))
  expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = "p"),
    list(command = "append-message", message = list(description = "Warning while evaluating test: careful", format = "plain")),
    list(command = "append-message", message = list(description = plot_message, format = "html")),
    list(command = "close-test", generated = "", status = list(enum = "correct"))
  ))
})

test_that("testGGPlot accepts the same geom_pointrange plot", {
  judge <- local_judge_with(p = ggplot(mtcars, aes(cyl, mpg, ymin = mpg - 1, ymax = mpg + 1)) + geom_pointrange())

  feedback <- capture_feedback(judge, {
    testGGPlot("p", function(env) env$p, ggplot(mtcars, aes(cyl, mpg, ymin = mpg - 1, ymax = mpg + 1)) + geom_pointrange())
  })

  # Bug #142: testGGPlot fails on geom_pointrange and geom_crossbar layers
  expect_failure(expect_equal(feedback[[length(feedback)]]$status$enum, "correct"))
})
