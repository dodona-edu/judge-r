test_env <- new.env()

context <- function(description = NULL, testcases={}, preExec={}) {    
    get_reporter()$start_context(description)
    on.exit(get_reporter()$end_context())

    test_env$clean_env <- new.env(parent = parent.env(globalenv()))
    tryCatch(
        {
            eval(preExec, envir = test_env$clean_env)
            source(student_code, local = test_env$clean_env)
            result = test_env$clean_env
            eval(testcases)
        }, 
        error = function(e) {
            on.exit(get_reporter()$add_message(paste("Error while evaluating context: ", conditionMessage(e), sep = '')), add = TRUE)
            on.exit(get_reporter()$escalate("compilation error"), add = TRUE)
        },
        warning = function(w) {            
            on.exit(get_reporter()$add_message(paste("Warning while evaluating context: ", w, sep = '')), add = TRUE)
        },
        message = function(m) {            
            on.exit(get_reporter()$add_message(paste("Message while evaluating context: ", m, sep = '')), add = TRUE)
        })
}