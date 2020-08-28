testEqual <- function(description, generated, expected, comparator = NULL, ...) {
    get_reporter()$start_test(expected, description)

    tryCatch(
        withCallingHandlers(
            {
                expected_val <- expected
                generated_val <- generated(test_env$clean_env)

                equal <- FALSE
                if (is.null(comparator)) {
                    equal <- isTRUE(all.equal(generated_val, expected_val, ...))
                } else {
                    equal <- comparator(generated_val, expected_val, ...)
                }

                if (equal) {
                    get_reporter()$end_test(generated_val, "correct")
                } else {
                    get_reporter()$end_test(generated_val, "wrong")
                }
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$end_test("", "wrong")
            get_reporter()$start_test("", description)
            get_reporter()$end_test(conditionMessage(e), "runtime error")
        }
    )
}

testGGPlot <- function(description, generated, expected, test_data = TRUE, test_aes = TRUE, test_geom = TRUE, ...) {
    get_reporter()$start_test("", description)

    tryCatch(
        withCallingHandlers(
            {
                expected_val <- expected
                generated_val <- generated(test_env$clean_env)

                equal <- TRUE
                if (test_data) {
                    test_data_result <- test_data_layer(expected_val$data, generated_val$data)
                    if (!test_data_result$equal) {
                        feedback <- test_data_result$feedback
                        equal <- FALSE
                    }
                }
                # Dont execute if a difference was already found in one of the previous layers
                if (test_aes && equal) {
                    test_aes_result <- test_aes_layer(expected_val$mapping, generated_val$mapping)
                    if (!test_aes_result$equal) {
                        feedback <- test_aes_result$feedback
                        equal <- FALSE
                    }
                }

                if (test_geom && equal) {
                    test_geom_result <- test_geom_layer(expected_val$layers, generated_val$layers)
                    if (!test_geom_result$equal) {
                        feedback <- test_geom_result$feedback
                        equal <- FALSE
                    }
                }

                if (equal) {
                    get_reporter()$end_test("", "correct")
                } else {
                    get_reporter()$add_message(feedback)
                    get_reporter()$end_test("", "wrong")
                }
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$end_test("", "wrong")
            get_reporter()$start_test("", description)
            get_reporter()$end_test(conditionMessage(e), "runtime error")
        }
    )
}

testIdentical <- function(description, generated, expected, ...) {
    get_reporter()$start_test(expected, description)

    tryCatch(
        withCallingHandlers(
            {
                expected_val <- expected
                generated_val <- generated(test_env$clean_env)

                equal <- isTRUE(identical(generated_val, expected_val, ...))

                if (equal) {
                    get_reporter()$end_test(generated_val, "correct")
                } else {
                    get_reporter()$end_test(generated_val, "wrong")
                }
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$end_test("", "wrong")
            get_reporter()$start_test("", description)
            get_reporter()$end_test(conditionMessage(e), "runtime error")
        }
    )
}

testImage <- function(generate, failIfAbsent = TRUE) {
    png(tf <- tempfile(fileext = ".png"))
    tryCatch(
        withCallingHandlers(
            {
                generate(test_env$clean_env)
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating image for testcase: ", conditionMessage(w), sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating image for testcase: ", conditionMessage(m), sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$start_test("", "")
            get_reporter()$end_test(conditionMessage(e), "runtime error")
        }
    )
    dev.off()
    if(file.exists(tf)) {
        image <- base64enc::base64encode(tf)
        get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image, "\"/>", sep=''), type = "html")
    } else if(failIfAbsent) {
        get_reporter()$start_test("", "")
        get_reporter()$end_test("We expected an image, but it doesn't seem like the code generated one.", "wrong")
    } else {
        get_reporter()$add_message("We expected an image, but it doesn't seem like the code generated one.")
    }
}
