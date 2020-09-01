testcase <- function(description = NULL, tests={}) {
    get_reporter()$start_testcase(description)
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            get_reporter()$end_testcase()
        }
    })

    tryCatch(
        {
            # Run the tests in the teacherEnvir
            eval(substitute(tests), envir=teacherEnvir)
        },
        error = function(e) {
            get_reporter()$add_message(paste("Error while evaluating testcase: ", conditionMessage(e), sep = ''))
            get_reporter()$end_testcase(accepted = FALSE)
            do_exit <<- FALSE
        }
    )
}
