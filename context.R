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
    .evaluate_context(testcases, substitute(preExec), .parse_student_r)
}

contextWithRmd <- function(testcases={}, preExec={}) {
    .evaluate_context(testcases, substitute(preExec), .parse_student_rmd)
}

contextWithImage <- function(testcases={}, preExec={}, failIfAbsent = TRUE, ...) {
    png(tf <- tempfile(fileext = ".png"), ...)
    .evaluate_context(testcases, substitute(preExec), .parse_student_r, image_file = tf, fail_if_absent = failIfAbsent)
}

# We don't use source, because otherwise syntax errors leak the location of the student code
.parse_student_r <- function() {
    parse(text = read_lines(student_code))
}

.parse_student_rmd <- function() {
    parse(text = knitr::purl(text = read_lines(student_code), quiet=TRUE))
}

# `testcases` must stay a promise: it is forced only after the student code ran.
# With `image_file`, a png device writing to it must already be open.
.evaluate_context <- function(testcases, preExec, parse_student_code, image_file = NULL, fail_if_absent = TRUE) {
    caller <- parent.frame()
    get_reporter()$start_context()
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            if(!is.null(image_file)) {
                .report_image(image_file, fail_if_absent, within_testcase = FALSE)
            }
            get_reporter()$end_context()
        }
    })

    test_env$clean_env <- new.env(parent = globalenv())
    tryCatch(
             .report_conditions("context", {
                 old_parent <- parent.env(.GlobalEnv)
                 parent.env(.GlobalEnv) <- starting_parent_env
                 tryCatch({
                     eval(preExec, envir = test_env$clean_env)
                     test_env$parsed_code <- parse_student_code()
                     capture.output(assign("evaluationResult", eval(test_env$parsed_code, envir = test_env$clean_env), envir = test_env$clean_env))
                 }, finally = {
                     if(!is.null(image_file)) {
                         dev.off()
                     }
                     parent.env(.GlobalEnv) <- old_parent
                 })
                 # eval() also evaluates the block's value: do that in the public function's frame.
                 eval(testcases, envir = caller)
             }),
             error = function(e) {
                 .add_condition_message("Error", "context", e)
                 get_reporter()$escalate("compilation error")
                 get_reporter()$end_context(accepted = FALSE)
                 do_exit <<- FALSE
             }
    )
}
