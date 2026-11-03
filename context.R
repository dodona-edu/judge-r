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

# Evaluates `expr`, which runs student code, with the JIT level student code had before the judge turned it off.
.with_student_jit <- function(expr) {
    old <- compiler::enableJIT(student_jit_level)
    on.exit(compiler::enableJIT(old))
    expr
}

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
# With `image_file`, the current device must be a png device writing to it.
.evaluate_context <- function(testcases, preExec, parse_student_code, image_file = NULL, fail_if_absent = TRUE) {
    caller <- parent.frame()
    image_device <- dev.cur()
    get_reporter()$start_context()
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            if(!is.null(image_file)) {
                .report_image(image_file, fail_if_absent, within_testcase = FALSE)
            }
            get_reporter()$end_context()
        }
        unlink(image_file)
    })

    test_env$clean_env <- new.env(parent = globalenv())
    error_status <- "internal error"
    tryCatch(
             .report_conditions("context", {
                 old_parent <- parent.env(.GlobalEnv)
                 parent.env(.GlobalEnv) <- starting_parent_env
                 tryCatch({
                     eval(preExec, envir = test_env$clean_env)
                     preexec_attached <- .preexec_attached(starting_parent_env)
                     error_status <- "compilation error"
                     test_env$parsed_code <- parse_student_code()
                     error_status <- "runtime error"
                     .report_captured_conditions("context", capture.output(assign("evaluationResult", .with_student_jit(eval(test_env$parsed_code, envir = test_env$clean_env)), envir = test_env$clean_env)))
                     error_status <- "internal error"
                 }, finally = {
                     # The student code may have closed it with dev.off() already.
                     if(!is.null(image_file) && image_device %in% dev.list()) {
                         dev.off(image_device)
                     }
                     parent.env(.GlobalEnv) <- old_parent
                 })
                 tryCatch({
                     .attach_for_testcases(preexec_attached)
                     # eval() also evaluates the block's value: do that in the public function's frame.
                     eval(testcases, envir = caller)
                 }, finally = .detach_for_testcases(preexec_attached))
             }),
             error = function(e) {
                 .add_condition_message("Error", "context", e)
                 get_reporter()$escalate(error_status)
                 get_reporter()$end_context(accepted = FALSE)
                 do_exit <<- FALSE
             }
    )
}

# What preExec attached to the search path, top first. Data is copied now, before the
# student code runs, so the student code cannot change what the testcases see.
.preexec_attached <- function(bottom) {
    attached <- list()
    env <- parent.env(.GlobalEnv)
    while (!.is_bottom_or_below(env, bottom)) {
        name <- attr(env, "name")
        if (is.null(name)) name <- "unnamed"
        is_package <- startsWith(name, "package:")
        attached[[length(attached) + 1]] <- list(
            name = paste0("preExec:", name),
            is_package = is_package,
            # Locked, so the student code cannot change it; attach() copies it without forcing its lazy data.
            values = if (is_package) env else as.list(env, all.names = TRUE)
        )
        env <- parent.env(env)
    }
    attached
}

# TRUE for `bottom` and the environments below it (Autoloads, base, the empty environment).
.is_bottom_or_below <- function(env, bottom) {
    repeat {
        if (identical(env, bottom)) return(TRUE)
        if (identical(bottom, emptyenv())) return(FALSE)
        bottom <- parent.env(bottom)
    }
}

# Data goes on top, where attach() in preExec put it. Packages go right above R's default
# packages, so the packages of the evaluation file win a name clash.
# attach() and detach() flush R's global variable cache for their names; a parent.env() swap does not.
.attach_for_testcases <- function(attached) {
    for (entry in rev(attached)) {
        if (!entry$is_package) {
            attach(entry$values, pos = 2L, name = entry$name, warn.conflicts = FALSE)
        }
    }
    for (entry in attached) {
        if (entry$is_package) {
            attach(entry$values, pos = .search_position(starting_parent_env),
                   name = entry$name, warn.conflicts = FALSE)
        }
    }
}

# The search path position of `env`, or right above package:base when `env` was detached.
.search_position <- function(env) {
    for (pos in seq_along(search())) {
        if (identical(pos.to.env(pos), env)) return(pos)
    }
    length(search())
}

.detach_for_testcases <- function(attached) {
    for (entry in attached) {
        if (entry$name %in% search()) detach(entry$name, character.only = TRUE)
    }
}
