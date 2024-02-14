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
                 capture.output(generated_val <- generated(test_env$clean_env))

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


testDF <- function(description, generated, expected, comparator = NULL, ignore_row_order = TRUE, ignore_col_order = TRUE, ...) {
    tryCatch(
             withCallingHandlers({
                 capture.output(generated_val <- generated(test_env$clean_env))
                 # Format before comparison, this allows us to change the values below
                 expected_formatted <- paste(knitr::kable(head(expected), "simple"), collapse = '\n')
                 generated_formatted <- paste(knitr::kable(head(generated_val), "simple"), collapse = '\n')

                 equal <- FALSE
                 if (is.null(comparator)) {
                     if (ignore_col_order) {
                         expected <- expected[,order(colnames(expected))]
                         generated_val <- generated_val[,order(colnames(generated_val))]
                     }
                     if (ignore_row_order) {
                         equal <- isTRUE(compare_data_frames_row_independent(expected, generated_val, ...))
                     } else {
                         equal <- isTRUE(all.equal(generated_val, expected, ...))
                     }
                 } else {
                     equal <- comparator(generated_val, expected, ...)
                 }

                 get_reporter()$start_test(expected_formatted, description)
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
                 expected_formatted <- paste(knitr::kable(head(expected), "simple"), collapse = '\n')
                 get_reporter()$start_test(expected_formatted, description)
                 get_reporter()$end_test("", "wrong")
                 get_reporter()$start_test("", description)
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}


testDF__inReview__ <- function(description, generated, expected, comparator = NULL, ignore_col_order = TRUE, ignore_row_order = FALSE, rows_before_error = 2, rows_after_error = 2, ...){
    tryCatch(
             withCallingHandlers({

                 capture.output(generated_val <- generated(test_env$clean_env))

                 #frames for custom comparator or when dataframe is correct
                 df1_bad_frame <- 1:(rows_before_error + rows_after_error + 1)
                 df2_bad_frame <- df1_bad_frame
                 df2_cols <- names(expected)
                 feedback <- ""

                 if(!is.data.frame(generated_val)){
                     get_reporter()$start_test("", "")
                     get_reporter()$end_test("We expected an object of the class \"data.frame\" but none was found.", "wrong")
                     return()
                 }

                 if (is.null(comparator)) {
                     # Use the dplyr all_equal to compare dataframes
                     test_result <- dataframe_all_equal(generated_val, expected,
                                                        ignore_col_order = ignore_col_order,
                                                        ignore_row_order = ignore_row_order,
                                                        before_error = rows_before_error,
                                                        after_error = rows_after_error
                     )
                     equal <- test_result$equal
                     if(!equal){
                         df1_bad_frame <- test_result$df1_rows
                         df2_bad_frame <- test_result$df2_rows
                         df2_cols <- test_result$df2_cols
                         feedback <- test_result$feedback
                     }
                 } else {
                     equal <- comparator(generated_val, expected, ...)
                 }

                 generated_df_view <- dplyr::slice(generated_val, df1_bad_frame)
                 expected_df_view <- dplyr::slice(expected, df2_bad_frame)
                 expected_df_view <- expected_df_view[df2_cols]

                 get_reporter()$start_test(paste(knitr::kable(expected_df_view, "simple", row.names = FALSE), collapse = '\n'), description)
                 get_reporter()$add_message(paste0(nrow(generated_df_view), " of the ", nrow(generated_val), " generated rows are shown."))
                 if (equal) {
                     get_reporter()$end_test(paste(knitr::kable(generated_df_view, "simple", row.names = FALSE), collapse = '\n'), "correct")
                 } else {
                     get_reporter()$add_message(feedback)
                     if(ignore_row_order){
                         get_reporter()$add_message("> Note: Row order doesn't matter, the expected dataframe is just one of the possible solutions.", type = "markdown")
                     } else {
                         get_reporter()$add_message("> Note: Row order does matter, your dataframe has to match the the expected dataframe exactly.", type = "markdown")
                     }
                     get_reporter()$end_test(paste(knitr::kable(generated_df_view, "simple", row.names = FALSE), collapse = '\n'), "wrong")
                 }
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating test: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating test: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$start_test("", description)
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}

testGGPlot <- function(description, generated, expected, show_expected = TRUE, test_data = TRUE, test_geom = TRUE, test_facet = TRUE, test_label = FALSE, test_scale = FALSE, ...) {
    get_reporter()$start_test("", description)

    tryCatch(
             withCallingHandlers({
                 expected_gg <- expected
                 capture.output(generated_gg <- generated(test_env$clean_env))

                 plot_exists <- is.ggplot(generated_gg)
                 feedback <- "You did not generate a ggplot"
                 if (plot_exists) {
                     generated_gg$labels$title <- paste(generated_gg$labels$title, "(Generated Plot)")
                     suppressMessages(ggsave(tf_generated <- tempfile(fileext = ".png"), plot = generated_gg, dpi = "screen"))
                 }

                 if (show_expected && plot_exists) { #we don't show the solution if the exercise is left empty
                     expected_gg$labels$title <- paste(expected_gg$labels$title, "(Expected Plot)")
                     suppressMessages(ggsave(tf_expected <- tempfile(fileext = ".png"), plot = expected_gg, dpi = "screen"))
                 }

                 equal <- plot_exists
                 if (test_data && equal) {
                     test_data_result <- test_data_layer(expected_gg$data, generated_gg$data)
                     if (!test_data_result$equal) {
                         feedback <- test_data_result$feedback
                         equal <- FALSE
                     }
                 }

                 if (test_label && equal) {
                     expected_labels <- expected_gg$labels
                     generated_labels <- generated_gg$labels
                     if (!isTRUE(all.equal(expected_labels$x, generated_labels$x)) |
                         !isTRUE(all.equal(expected_labels$y, generated_labels$y))){
                         feedback <- "Did you specify the correct labels?"
                         equal <- FALSE
                     }
                 }

                 if (test_facet && equal) {
                     test_facet_result <- test_facet_layer(expected_gg$facet, generated_gg$facet, ...)
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

                 if (plot_exists) {
                     image_generated <- base64enc::base64encode(tf_generated)
                     get_reporter()$add_message(paste("<img style=\"max-width:100%; width:450px;\" src=\"data:image/png;base64,", image_generated, "\"/>", sep=''), type = "html")
                 }
                 if (equal) {
                     get_reporter()$end_test("", "correct")
                 } else {
                     if (show_expected && plot_exists) {
                         image_expected <- base64enc::base64encode(tf_expected)
                         get_reporter()$add_message(paste("<img style=\"max-width:100%; width:450px;\" src=\"data:image/png;base64,", image_expected, "\"/>", sep=''), type = "html")
                     }
                     get_reporter()$add_message(feedback)
                     get_reporter()$end_test("", "wrong")
                 }
                 if (show_expected && plot_exists) {
                     file.remove(tf_expected)
                 }
                 if (plot_exists) {
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

testImage <- function(generate, failIfAbsent = TRUE, ...) {
    png(tf <- tempfile(fileext = ".png"), ...)
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
    if (file.exists(tf)) {
        image <- base64enc::base64encode(tf)
        get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image, "\"/>", sep=''), type = "html")
    } else if (failIfAbsent) {
        get_reporter()$start_test("", "")
        get_reporter()$end_test("We expected an image, but it doesn't seem like the code generated one.", "wrong")
    } else {
        get_reporter()$add_message("We expected an image, but it doesn't seem like the code generated one.")
    }
}

testFunctionUsedInVar <- function(funcName, varName){
    assignment_paths <- find_assign(test_env$parsed_code)
    if (varName %in% names(assignment_paths)) {
        used <- is_function_used_in_var(funcName, assignment_paths, varName, test_env$parsed_code)
        if (used) {
            get_reporter()$add_message(paste0("You used the \"", funcName, "\" function in an assignment to variable \"",  varName, "\"."))
        } else {
            get_reporter()$start_test("", "")
            get_reporter()$end_test(paste0("We expected you to use the \"", funcName, "\" function in an assignment to variable \"",  varName, "\"."), "wrong")
        }
    } else {
        get_reporter()$start_test("", "")
        get_reporter()$end_test(paste0("Variable \"",  varName, "\" not found"), "wrong")
    }
}

testFunctionUsed <- function(funcName){
    assignment_paths <- find_assign(test_env$parsed_code)
    used <- is_function_used(funcName, assignment_paths, test_env$parsed_code, test_env$parsed_code)
    if (used) {
        get_reporter()$add_message(paste("You used the \"", funcName, "\" function in your code."))
    } else {
        get_reporter()$start_test("", "")
        get_reporter()$end_test(paste("We expected you to use the \"", funcName, "\" function in your code."), "wrong")
    }
}

testHtest <- function(description, generated, expected,
                      test_p_value = TRUE,
                      test_interval = TRUE,
                      test_statistic = FALSE,
                      test_alternative = FALSE,
                      test_confidence_level = FALSE,
                      test_method = FALSE
                      ){

    tryCatch(
             withCallingHandlers({
                 expected_val <- expected
                 capture.output(generated_val <- generated(test_env$clean_env))

                 if (!"htest" %in% class(generated_val)) {
                     get_reporter()$start_test("", "")
                     get_reporter()$end_test(
                                             paste0("We expected an object of the class \"htest\" but none was found. ",
                                                    "This is the class of objects generated by functions that perform ",
                                                    "hypothesis tests (e.g., the R function t.test)"),
                                             "wrong")
                     return()
                 }

                 expected_formatted <- ""
                 generated_formatted <- ""

                 equal <- TRUE
                 if (test_p_value) {
                     equal <- equal && isTRUE(all.equal(generated_val$p.value, expected_val$p.value))
                     expected_formatted <- paste0(expected_formatted, "\np-value = ", expected_val$p.value)
                     generated_formatted <- paste0(generated_formatted, "\np-value = ", generated_val$p.value)
                 }
                 if (test_interval) {
                     equal <- equal && isTRUE(all.equal(generated_val$conf.int, expected_val$conf.int))
                     expected_formatted <- paste0(expected_formatted, "\nconfidence interval = ", toString(expected_val$conf.int))
                     generated_formatted <- paste0(generated_formatted, "\nconfidence interval = ", toString(generated_val$conf.int))
                 }
                 if (test_statistic) {
                     equal <- equal && isTRUE(all.equal(generated_val$statistic, expected_val$statistic))
                     expected_formatted <- paste0(expected_formatted, "\ntest statistic = ")
                     generated_formatted <- paste0(generated_formatted, "\ntest statistic = ")
                     for (statistic in names(expected_val$statistic)){
                         expected_formatted <- paste0(expected_formatted, "\n\t", statistic, ": ", expected_val$statistic[[statistic]])
                     }
                     for (statistic in names(generated_val$statistic)){
                         generated_formatted <- paste0(generated_formatted, "\n\t", statistic, ": ", generated_val$statistic[[statistic]])
                     }
                 }
                 if (test_alternative) {
                     equal <- equal && isTRUE(all.equal(generated_val$alternative, expected_val$alternative))
                     expected_formatted <- paste0(expected_formatted, "\nalternative = ", expected_val$alternative)
                     generated_formatted <- paste0(generated_formatted, "\nalternative = ", generated_val$alternative)
                 }
                 if (test_confidence_level) {
                     equal <- equal && isTRUE(all.equal(attr(generated_val$conf.int,'conf.level'), attr(expected_val$conf.int,'conf.level')))
                     expected_formatted <- paste0(expected_formatted, "\nconfidence level = ", attr(expected_val$conf.int,'conf.level'))
                     generated_formatted <- paste0(generated_formatted, "\nconfidence level = ", attr(generated_val$conf.int,'conf.level'))
                 }
                 if (test_method) {
                     equal <- equal && isTRUE(all.equal(generated_val$method, expected_val$method))
                     expected_formatted <- paste0(expected_formatted, "\nmethod = ", expected_val$method)
                     generated_formatted <- paste0(generated_formatted, "\nmethod = ", generated_val$method)
                 }

                 get_reporter()$start_test(expected_formatted, description)
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
                 get_reporter()$start_test("", description)
                 get_reporter()$end_test(conditionMessage(e), "runtime error")
             }
    )
}

testMultipleChoice <- function(description, generated, expected, possible_answers,
                               verify_answer = FALSE,
                               give_feedback = TRUE,
                               feedback = NULL,
                               show_expected = FALSE) {

    get_reporter()$start_test(ifelse(show_expected, expected, "●"), description)

    tryCatch(
             withCallingHandlers({
                 expected_val <- unique(sort(expected))
                 capture.output(generated_raw <- generated(test_env$clean_env))
                 generated_val <- unique(sort(generated_raw))

                 equal <- TRUE
                 if (!all(generated_val %in% possible_answers) || length(generated_val) == 0) {
                     equal <- FALSE
                     get_reporter()$add_message(paste0("Your answer is not a valid option, the valid options are (", toString(possible_answers), ")."))
                 }

                 if (verify_answer && equal) {
                     feedback_res <- ""
                     for (answer in generated_val){
                         if (!(answer %in% expected_val)){
                             equal <- FALSE
                             feedback_res <- paste0(feedback_res, "\n", answer, " is not a right answer")
                             if (answer %in% names(feedback) || answer %in% seq_along(feedback)) {
                                 feedback_res <- paste0(feedback_res, " because: ", feedback[[answer]])
                             } else {
                                 feedback_res <- paste0(feedback_res, ".")
                             }
                         }
                     }
                     if (length(intersect(expected_val, generated_val)) < length(expected_val)) {
                         equal <- FALSE
                         feedback_res <- paste0(feedback_res, "\nYour answer does not include all the correct options.")
                     }
                     if (feedback_res != "" && !equal && give_feedback){
                         get_reporter()$add_message(feedback_res)
                     }
                 } else if (!verify_answer && equal){
                     get_reporter()$add_message("Your solution will be verified after the deadline.")
                 }

                 if (equal) {
                     get_reporter()$end_test(generated_raw, "correct")
                 } else {
                     get_reporter()$end_test(generated_raw, "wrong")
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
