test_env <- new.env()

read_lines <- function(file) {
    con <- file(student_code, "r")
    on.exit(close(con))
    lines <- readLines(con, warn = FALSE)
}

context <- function(testcases={}, preExec={}) {    
    get_reporter()$start_context()
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            get_reporter()$end_context()
        }
    })

    test_env$clean_env <- new.env(parent = globalenv())
    tryCatch(
        withCallingHandlers(
            {
                eval(preExec, envir = test_env$clean_env)
                # We don't use source, because otherwise syntax errors leak the location of the student code
                eval(parse(text = read_lines(student_code)), envir = test_env$clean_env)
                result = test_env$clean_env
                eval(testcases)
            },
            warning = function(w) {
                get_reporter()$add_message(paste("Warning while evaluating context: ", conditionMessage(w), sep = ''))
            },
            message = function(m) {
                get_reporter()$add_message(paste("Message while evaluating context: ", conditionMessage(m), sep = ''))
            }
        ),
        error = function(e) {
            get_reporter()$add_message(paste("Error while evaluating context: ", conditionMessage(e), sep = ''))
            get_reporter()$escalate("compilation error")
            get_reporter()$end_context(accepted = FALSE)
            do_exit <<- FALSE
        }
    )
}