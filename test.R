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
                
                expected_html <- knitr::kable(head(expected), "html", table.attr = "class=\"table\"")
                generated_html <- knitr::kable(head(generated_val), "html", table.attr = "class=\"table\"")

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

testGGPlot <- function(description, generated, expected, test_data = TRUE, test_aes = TRUE, test_geom = TRUE, ...) {
    get_reporter()$start_test("", description)

    tryCatch(
        withCallingHandlers(
            {
                expected_gg <- expected
                generated_gg <- generated(test_env$clean_env)
                
                expected_gg$labels$title <- paste(expected_gg$labels$title, "(Expected Plot)")
                generated_gg$labels$title <- paste(generated_gg$labels$title, "(Generated Plot)")

                ggsave(tf_expected <- tempfile(fileext = ".png"), plot = expected_gg)
                ggsave(tf_generated <- tempfile(fileext = ".png"), plot = generated_gg)

                equal <- TRUE
                if (test_data) {
                    test_data_result <- test_data_layer(expected_gg$data, generated_gg$data)
                    if (!test_data_result$equal) {
                        feedback <- test_data_result$feedback
                        equal <- FALSE
                    }
                }
                # Dont execute if a difference was already found in one of the previous layers
                if (test_aes && equal) {
                    test_aes_result <- test_aes_layer(expected_gg$mapping, generated_gg$mapping)
                    if (!test_aes_result$equal) {
                        feedback <- test_aes_result$feedback
                        equal <- FALSE
                    }
                }

                if (test_geom && equal) {
                    test_geom_result <- test_geom_layer(expected_gg$layers, generated_gg$layers)
                    if (!test_geom_result$equal) {
                        feedback <- test_geom_result$feedback
                        equal <- FALSE
                    }
                }

                if (equal) {
                    image_generated <- base64enc::base64encode(tf_generated)
                    get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image_generated, "\"/>", sep=''), type = "html")
                    get_reporter()$end_test("", "correct")
                } else {
                    image_generated <- base64enc::base64encode(tf_generated)
                    get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image_generated, "\"/>", sep=''), type = "html")
                    image_expected <- base64enc::base64encode(tf_expected)
                    get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image_expected, "\"/>", sep=''), type = "html")
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

testFunctionUsedInVar <- function(funcName, varName){
    tryCatch(
        withCallingHandlers(
            {
                assignment_paths <- find_assign(test_env$parsed_code)
                used <- is_function_used_in_var(funcName, assignment_paths, varName, test_env$parsed_code)
                if (used) {
                    get_reporter()$add_message(
                        paste("You used the \"", funcName, "\" function in an assignment to variable \"",  varName, "\".")
                    )
                } else {
                    get_reporter()$start_test("", "")
                    get_reporter()$end_test(
                        paste("We expected you to use the \"", funcName, "\" function in an assignment to variable \"",  varName, "\"."),
                        "wrong"
                    )
                }
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
}

testFunctionUsed <- function(funcName){
    tryCatch(
        withCallingHandlers(
            {
                assignment_paths <- find_assign(test_env$parsed_code)
                used <- is_function_used(funcName, assignment_paths, test_env$parsed_code, test_env$parsed_code)
                if (used) {
                    get_reporter()$add_message(paste("You used the \"", funcName, "\" function in your code."))
                } else {
                    get_reporter()$start_test("", "")
                    get_reporter()$end_test(paste("We expected you to use the \"", funcName, "\" function in your code."), "wrong")
                }
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
}
