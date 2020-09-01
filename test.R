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

testDF <- function(description, generated, expected, comparator = NULL, ignore_col_order=TRUE, ignore_row_order=TRUE, ...) {
    # Convert first 5 lines of dataframe to HTML
    get_reporter()$start_test("", description)

    tryCatch(
        withCallingHandlers(
            {
                expected_val <- expected
                generated_val <- generated(test_env$clean_env)
                # Convert first 5 lines of dataframe to HTML
                
                expected_html <- knitr::kable(head(expected), "html", table.attr = "style='border-spacing: 10px 0;'")
                generated_html <- knitr::kable(head(generated_val), "html", table.attr = "style='border-spacing: 10px 0;'")

                equal <- FALSE
                if (is.null(comparator)) {
                    # use the dplyr all_equal to compare dataframes, ignoring row/col order by default
                    equal <- isTRUE(dplyr::all_equal(generated_val, expected_val, ignore_col_order=ignore_col_order, ignore_row_order=ignore_row_order, ...))
                } else {
                    equal <- comparator(generated_val, expected_val, ...)
                }

                if (equal) {
                    get_reporter()$add_message(generated_html, type = "html")
                    get_reporter()$end_test("", "correct")
                } else {
                    get_reporter()$add_message(paste("Generated",generated_html,sep = '\n'), type = "html")
                    get_reporter()$add_message(paste("Expected",expected_html,sep = '\n'), type = "html")
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
