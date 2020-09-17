testEqual <- function(description, generated, expected, comparator = NULL, formatter = NULL, ...) {
    if (is.null(formatter)) {
        expected_formatted <- expected
    } else {
        expected_formatted = formatter(expected)
    }
    get_reporter()$start_test(expected_formatted, description)

    tryCatch(
             withCallingHandlers({
                 expected_val <- expected
                 generated_val <- generated(test_env$clean_env)

                 equal <- FALSE
                 if (is.null(comparator)) {
                     equal <- isTRUE(all.equal(generated_val, expected_val, ...))
                 } else {
                     equal <- comparator(generated_val, expected_val, ...)
                 }

                 if (is.null(formatter)) {
                     generated_formatted <- generated_val
                 } else {
                     generated_formatted <- formatter(generated_val)
                 }
                 if (equal) {
                     get_reporter()$end_test(generated_formatted, "correct")
                 } else {
                     get_reporter()$end_test(generated_formatted, "wrong")
                 }
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$end_test("", "wrong")
                 get_reporter()$start_test("", description)
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}


testDF <- function(description, generated, expected, comparator = NULL, ...) {
    expected_formatted <- paste(knitr::kable(head(expected), "simple"), collapse = '\n')
    get_reporter()$start_test(expected_formatted, description)
    tryCatch(
             withCallingHandlers({

                 generated_val <- generated(test_env$clean_env)
                 generated_formatted <- paste(knitr::kable(head(generated_val), "simple"), collapse = '\n')

                 equal <- FALSE
                 if (is.null(comparator)) {
                     # Use the dplyr all_equal to compare dataframes
                     equal <- isTRUE(dplyr::all_equal(generated_val, expected, ...))
                 } else {
                     equal <- comparator(generated_val, expected, ...)
                 }

                 if (equal) {
                     get_reporter()$add_message("Only the first five rows of the dataframe are shown.")
                     get_reporter()$end_test(generated_formatted, "correct")
                 } else {
                     # TODO: Try to be smarter about what data is shown. If the error is not in the first five rows, the diff will be useless to the student.
                     get_reporter()$add_message("Only the first five rows of the dataframes are shown. If they are all equal, your mistake only shows up in the later rows.")
                     get_reporter()$end_test(generated_formatted, "wrong")
                 }
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$end_test("", "wrong")
                 get_reporter()$start_test("", description)
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}

testGGPlot <- function(description, generated, expected, show_expected = TRUE,
                        test_data = TRUE, 
                        test_geom = TRUE, 
                        test_facet = TRUE,
                        test_label = FALSE, 
                        test_scale = FALSE
                        ) {

    get_reporter()$start_test("", description)

    tryCatch(
             withCallingHandlers({
                 expected_gg <- expected
                 generated_gg <- generated(test_env$clean_env)

                 if(show_expected){
                    expected_gg$labels$title <- paste(expected_gg$labels$title, "(Expected Plot)")
                    suppressMessages(ggsave(tf_expected <- tempfile(fileext = ".png"), plot = expected_gg, dpi = "screen"))
                 }
                 generated_gg$labels$title <- paste(generated_gg$labels$title, "(Generated Plot)")
                 suppressMessages(ggsave(tf_generated <- tempfile(fileext = ".png"), plot = generated_gg, dpi = "screen"))

                 equal <- TRUE
                 if (test_data) {
                     test_data_result <- test_data_layer(expected_gg$data, generated_gg$data)
                     if (!test_data_result$equal) {
                         feedback <- test_data_result$feedback
                         equal <- FALSE
                     }
                 }

                 if (test_label) {
                     expected_labels <- expected_gg$labels
                     generated_labels <- generated_gg$labels
                     if (!isTRUE(all.equal(expected_labels$x, generated_labels$x)) |
                         !isTRUE(all.equal(expected_labels$y, generated_labels$y))){
                         feedback <- "Did you specify the correct labels?"
                         equal <- FALSE
                     }
                 }

                 if (test_facet && equal) {
                     test_facet_result <- test_facet_layer(expected_gg$facet, generated_gg$facet)
                     if (!test_facet_result$equal) {
                         feedback <- test_facet_result$feedback
                         equal <- FALSE
                     }
                 }

                 if (test_geom && equal) {
                     test_geom_result <- test_geom_layer(expected_gg, generated_gg)
                     if (!test_geom_result$equal) {
                         feedback <- test_geom_result$feedback
                         equal <- FALSE
                     }
                 }

                 if (test_scale && equal) {
                     expected_scale <- expected_gg$scales$scales
                     generated_scale <- generated_gg$scales$scales
                     if (!isTRUE(all.equal(expected_scale, generated_scale))) {
                         feedback <- "Did you specify the correct scales?"
                         equal <- FALSE
                     }
                 }

                 if (equal) {
                     image_generated <- base64enc::base64encode(tf_generated)
                     get_reporter()$add_message(paste("<img style=\"max-width:100%; width:450px;\" src=\"data:image/png;base64,", image_generated, "\"/>", sep=''), type = "html")
                     get_reporter()$end_test("", "correct")
                 } else {
                     image_generated <- base64enc::base64encode(tf_generated)
                     get_reporter()$add_message(paste("<img style=\"max-width:100%; width:450px;\" src=\"data:image/png;base64,", image_generated, "\"/>", sep=''), type = "html")
                     if(show_expected){
                        image_expected <- base64enc::base64encode(tf_expected)
                        get_reporter()$add_message(paste("<img style=\"max-width:100%; width:450px;\" src=\"data:image/png;base64,", image_expected, "\"/>", sep=''), type = "html")
                     }
                     get_reporter()$add_message(feedback)
                     get_reporter()$end_test("", "wrong")
                 }
                 if (show_expected && file.exists(tf_expected)){
                    file.remove(tf_expected)
                 }
                 if (file.exists(tf_generated)){
                     file.remove(tf_generated)
                 }                 
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$end_test("", "wrong")
                 get_reporter()$start_test("", description)
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}

testIdentical <- function(description, generated, expected, formatter = NULL, ...) {
    testEqual(
              description,
              generated,
              expected,
              comparator = function(generated_val, expected_val, ...) { isTRUE(identical(generated_val, expected_val, ...)) },
              formatter = formatter,
              ...
    )
}

testImage <- function(generate, failIfAbsent = TRUE) {
    png(tf <- tempfile(fileext = ".png"))
    tryCatch(
             withCallingHandlers({
                 generate(test_env$clean_env)
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating image for testcase: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating image for testcase: ", conditionMessage(m), sep = ''))
             }),
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
             withCallingHandlers({
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
             }),
             error = function(e) {
                 get_reporter()$start_test("", "")
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}

testFunctionUsed <- function(funcName){
    tryCatch(
             withCallingHandlers({
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
             }),
             error = function(e) {
                 get_reporter()$start_test("", "")
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}
