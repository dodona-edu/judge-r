testcase <- function(description = NULL, tests={}) {
    get_reporter()$start_testcase(description)
    on.exit(get_reporter()$end_testcase())

    tryCatch(
        {
            eval(tests)
        },
        error = function(e) {
            on.exit(get_reporter()$add_message(paste("Error while evaluating testcase: ", e, sep = '')), add = TRUE)
        }
    )
}