# We need to do some tricky stuff to make sure students can both load libraries
# and not actually use the libraries loaded by the test code. The reasons for
# this are two-fold:
#  1. When loading a library, R takes the global env and injects the library
#     between the global env and its parent env. This means that if we base a new
#     environment on something above the global env, loading a new library when
#     executing in that env doesn't actually make that library show up in the
#     environment we are currently executing in.
#  2. A number of libraries are loaded by default when starting R. This means
#     that when starting R, the parent of the global env is not in fact the
#     base env. These libraries also contain some pretty important functions
#     (e.g. `data`), so not having them in the student environment is not an
#     option.
# So, what do we do? When starting the judge, we immediately save the parent of
# the global environment to a variable (`starting_parent_env`). Right before we
# start executing in the student env, we (again) save the parent of the global
# env (`old_parent`) and then change the parent of the global env to
# `starting_parent_env`. When we stop executing in the student env, we change
# the parent of the global env back to `old_parent`. This makes sure that any
# test code using libraries can do so.


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
             withCallingHandlers({
                 old_parent <- parent.env(.GlobalEnv)
                 parent.env(.GlobalEnv) <- starting_parent_env
                 tryCatch({
                     eval(substitute(preExec), envir = test_env$clean_env)
                     # We don't use source, because otherwise syntax errors leak the location of the student code
                     test_env$parsed_code <- parse(text = read_lines(student_code))
                     capture.output(assign("evaluationResult", eval(test_env$parsed_code, envir = test_env$clean_env), envir = test_env$clean_env))
                 }, finally = {
                     parent.env(.GlobalEnv) <- old_parent
                 })
                 eval(testcases)
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating context: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating context: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$add_message(paste("Error while evaluating context: ", conditionMessage(e), sep = ''))
                 get_reporter()$escalate("compilation error")
                 get_reporter()$end_context(accepted = FALSE)
                 do_exit <<- FALSE
             }
    )
}

contextWithRmd <- function(testcases={}, preExec={}) {
    get_reporter()$start_context()
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            get_reporter()$end_context()
        }
    })

    test_env$clean_env <- new.env(parent = globalenv())
    tryCatch(
             withCallingHandlers({
                 old_parent <- parent.env(.GlobalEnv)
                 parent.env(.GlobalEnv) <- starting_parent_env
                 tryCatch({
                     eval(substitute(preExec), envir = test_env$clean_env)
                     # We don't use source, because otherwise syntax errors leak the location of the student code
                     test_env$parsed_code <- parse(text = knitr::purl(text = read_lines(student_code), quiet=TRUE))
                     capture.output(assign("evaluationResult", eval(test_env$parsed_code, envir = test_env$clean_env), envir = test_env$clean_env))
                 }, finally = {
                     parent.env(.GlobalEnv) <- old_parent
                 })
                 eval(testcases)
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating context: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating context: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$add_message(paste("Error while evaluating context: ", conditionMessage(e), sep = ''))
                 get_reporter()$escalate("compilation error")
                 get_reporter()$end_context(accepted = FALSE)
                 do_exit <<- FALSE
             }
    )
}

contextWithImage <- function(testcases={}, preExec={}, failIfAbsent = TRUE, ...) {
    png(tf <- tempfile(fileext = ".png"), ...)
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
             withCallingHandlers({
                 old_parent <- parent.env(.GlobalEnv)
                 parent.env(.GlobalEnv) <- starting_parent_env
                 tryCatch({
                     eval(substitute(preExec), envir = test_env$clean_env)
                     # We don't use source, because otherwise syntax errors leak the location of the student code
                     test_env$parsed_code <- parse(text = read_lines(student_code))
                     capture.output(assign("evaluationResult", eval(test_env$parsed_code, envir = test_env$clean_env), envir = test_env$clean_env))
                 }, finally = {
                     dev.off()
                     parent.env(.GlobalEnv) <- old_parent
                 })
                 eval(testcases)
             },
             warning = function(w) {
                 get_reporter()$add_message(paste("Warning while evaluating context: ", conditionMessage(w), sep = ''))
             },
             message = function(m) {
                 get_reporter()$add_message(paste("Message while evaluating context: ", conditionMessage(m), sep = ''))
             }),
             error = function(e) {
                 get_reporter()$add_message(paste("Error while evaluating context: ", conditionMessage(e), sep = ''))
                 get_reporter()$escalate("compilation error")
                 get_reporter()$end_context(accepted = FALSE)
                 do_exit <<- FALSE
             }
    )
}
