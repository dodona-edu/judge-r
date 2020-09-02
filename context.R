test_env <- new.env()

read_lines <- function(filename) {
    con <- file(filename, "r")
    on.exit(close(con))
    readLines(con, warn = FALSE)
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
                test_env$parsed_code <- parse(text = read_lines(student_code))
                assign(".Last.value",
                       eval(test_env$parsed_code, envir = test_env$clean_env),
                       envir = test_env$clean_env)
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

contextWithImage <- function(testcases={}, preExec={}, failIfAbsent = TRUE) {
    png(tf <- tempfile(fileext = ".png"))
    get_reporter()$start_context()
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            if(file.exists(tf)) {
                image <- base64enc::base64encode(tf)
                get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image, "\"/>", sep=''), type = "html")
            } else if(failIfAbsent) {
                get_reporter()$start_testcase("")
                get_reporter()$start_test("", "")
                get_reporter()$end_test("We expected an image, but it doesn't seem like the code generated one.", "wrong")
                get_reporter()$end_testcase()
            } else {
                get_reporter()$add_message("We expected an image, but it doesn't seem like the code generated one.")
            }
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
                # We need to do this here, since the testcases might generate more plots, so we need to write the images before then.
                dev.off()
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
