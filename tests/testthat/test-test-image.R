no_image <- "We expected an image, but it doesn't seem like the code generated one."

test_that("testImage shows the image generate draws", {
  local_tempdir_pngs()
  judge <- local_judge_with(x = 1:3)

  feedback <- capture_feedback(judge, testImage(function(env) plot(env$x)))

  expect_equal(hide_pngs(feedback), list(
    list(command = "append-message", message = list(description = "<img src=\"data:image/png;base64,<png>\"/>", format = "html"))
  ))
})

test_that("testImage fails a test when generate draws nothing", {
  local_tempdir_pngs()
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testImage(function(env) NULL))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = no_image, status = list(enum = "wrong"))
  ))
})

test_that("testImage only adds a message when generate draws nothing with failIfAbsent = FALSE", {
  local_tempdir_pngs()
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testImage(function(env) NULL, failIfAbsent = FALSE))

  expect_equal(feedback, list(
    list(command = "append-message", message = list(
      description = "We expected an image, but it doesn't seem like the code generated one.",
      format = "plain"
    ))
  ))
})

test_that("testImage passes extra arguments to png", {
  local_tempdir_pngs()
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testImage(function(env) plot(1), width = 300, height = 200))
  image <- base64enc::base64decode(sub(".*base64,(.*)\"/>", "\\1", feedback[[1]]$message$description))

  # A PNG stores its width and height in bytes 17 to 24.
  expect_equal(readBin(image[17:24], "integer", n = 2, size = 4, endian = "big"), c(300L, 200L))
})

test_that("testImage reports an error in generate as a runtime error", {
  local_tempdir_pngs()
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, testImage(function(env) stop("boom")))

  expect_equal(feedback, list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error")),
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = no_image, status = list(enum = "wrong"))
  ))
})

test_that("testImage shows what generate drew before an error", {
  local_tempdir_pngs()
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testImage(function(env) {
      plot(1)
      stop("boom")
    })
  })

  expect_equal(hide_pngs(feedback), list(
    list(command = "start-test", expected = "", description = ""),
    list(command = "close-test", generated = "boom", status = list(enum = "runtime error")),
    list(command = "append-message", message = list(description = "<img src=\"data:image/png;base64,<png>\"/>", format = "html"))
  ))
})

test_that("testImage reports a warning in generate", {
  local_tempdir_pngs()
  judge <- local_judge_with()

  feedback <- capture_feedback(judge, {
    testImage(function(env) {
      plot(1)
      warning("careful")
    })
  })

  expect_equal(hide_pngs(feedback), list(
    list(command = "append-message", message = list(description = "Warning while evaluating image for testcase: careful", format = "plain")),
    list(command = "append-message", message = list(description = "<img src=\"data:image/png;base64,<png>\"/>", format = "html"))
  ))
})

test_that("testImage leaves the current graphics device as it was", {
  local_tempdir_pngs()
  judge <- local_judge_with()
  device <- dev.cur()

  capture_feedback(judge, testImage(function(env) plot(1)))
  expect_equal(dev.cur(), device)

  capture_feedback(judge, testImage(function(env) stop("boom")))
  expect_equal(dev.cur(), device)
})

test_that("testImage removes the image it saves", {
  local_tempdir_pngs()
  judge <- local_judge_with()
  before <- tempdir_pngs()

  feedback <- capture_feedback(judge, testImage(function(env) plot(1)))

  expect_length(feedback, 1)
  # Bug #120: testImage and testGGPlot leave PNG files in the temporary directory
  expect_failure(expect_equal(tempdir_pngs(), before))
})
