testcase <- function(description = NULL, tests={}) {
    get_reporter()$start_testcase(description)
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            get_reporter()$end_testcase()
        }
    })

    tryCatch({ eval(tests) }, error = function(e) {
        get_reporter()$add_message(paste("Error while evaluating testcase: ", conditionMessage(e), sep = ''))
        get_reporter()$escalate("runtime error")
        get_reporter()$end_testcase(accepted = FALSE)
        do_exit <<- FALSE
    })
}

testcaseAssert <- function(description, checker) {
    get_reporter()$start_testcase(description)
    tryCatch(
        withCallingHandlers(
            {
                capture.output(checker_val <- checker(test_env$clean_env))
                if (!checker_val) {
                    get_reporter()$escalate("wrong")
                }
                get_reporter()$end_testcase(accepted = checker_val)
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating assert: ", conditionMessage(w), sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating assert: ", conditionMessage(m), sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$add_message(paste("Error while evaluating assert: ", conditionMessage(e), sep = ''))
            get_reporter()$escalate("runtime error")
            get_reporter()$end_testcase(accepted = FALSE)
        }
    )
}
