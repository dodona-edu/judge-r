test_env <- new.env()

context <- function(testcases={}, preExec={}) {    
    get_reporter()$start_context()
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            get_reporter()$end_context()
        }
    })

    test_env$clean_env <- new.env(parent = parent.env(globalenv()))
    tryCatch(
        withCallingHandlers(
            {
                eval(preExec, envir = test_env$clean_env)
                source(student_code, local = test_env$clean_env)
                result = test_env$clean_env
                eval(testcases)
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating context: ", w, sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating context: ", m, sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$add_message(paste("Error while evaluating context: ", e, sep = ''))
            get_reporter()$escalate("compilation error")
            get_reporter()$end_context(accepted = FALSE)
            do_exit <<- FALSE
        }
    )
}