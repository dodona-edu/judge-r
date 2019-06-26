test_equal <- function(description, generated, expected, comparator = NULL, ...) {
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
                    equal <- comparator(generated, expected, ...)
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