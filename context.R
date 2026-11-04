# Student code and preExec run with .GlobalEnv's parent set to R's default packages; the testcases run with the
# judge's and the evaluation file's packages on the search path.
# README.md, section "Contexts" ("What each piece of code can see"), has the full picture.


test_env <- new.env()

# R's default packages, from starting_parent_env down to base, as they were linked before any student code ran.
default_packages <- local({
    envs <- list(starting_parent_env)
    while (!identical(envs[[length(envs)]], baseenv())) {
        envs[[length(envs) + 1]] <- parent.env(envs[[length(envs)]])
    }
    envs
})

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
                     .flush_cached(old_parent)
                     eval(preExec, envir = test_env$clean_env)
                     preexec_attached <- .preexec_attached(starting_parent_env)
                     error_status <- "compilation error"
                     test_env$parsed_code <- parse_student_code()
                     error_status <- "runtime error"
                     .report_captured_conditions("context", capture.output(assign("evaluationResult", .with_student_jit(eval(test_env$parsed_code, envir = test_env$clean_env)), envir = test_env$clean_env)))
                     error_status <- "internal error"
                 }, finally = {
                     restored <- .restore_default_packages()
                     # The student code may have closed it with dev.off() already.
                     if(!is.null(image_file) && image_device %in% dev.list()) {
                         dev.off(image_device)
                     }
                     student_parent <- parent.env(.GlobalEnv)
                     parent.env(.GlobalEnv) <- old_parent
                     .flush_cached(student_parent, old_parent, names = restored)
                     .keep_student_attached(student_parent)
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

# Data goes on top, where attach() in preExec put it. Packages go right above the judge's
# packages: on a name clash they lose to the evaluation file's packages and win over the judge's.
# attach() and detach() flush R's global variable cache for their names; a parent.env() swap does not.
.attach_for_testcases <- function(attached) {
    for (entry in rev(attached)) {
        if (!entry$is_package) {
            attach(entry$values, pos = 2L, name = entry$name, warn.conflicts = FALSE)
        }
    }
    for (entry in attached) {
        if (entry$is_package) {
            attach(entry$values, pos = .search_position(judge_parent_env),
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

# parent.env<- does not flush R's global variable cache; defining a name in an attached environment does.
.flush_cached <- function(..., names = character()) {
    for (env in list(...)) {
        while (!.is_bottom_or_below(env, starting_parent_env)) {
            names <- c(names, ls(env, all.names = TRUE))
            env <- parent.env(env)
        }
    }
    flush <- attach(NULL, name = "judge:flush", warn.conflicts = FALSE)
    on.exit(detach("judge:flush", character.only = TRUE))
    for (name in unique(names)) assign(name, NULL, envir = flush)
}

# Links back the default packages the student code detached (detach() points them at base); returns the names to flush.
.restore_default_packages <- function() {
    names <- character()
    for (i in seq_len(length(default_packages) - 1)) {
        env <- default_packages[[i]]
        below <- default_packages[[i + 1]]
        if (!.is_bottom_or_below(below, parent.env(env))) {
            parent.env(env) <- below
            names <- c(names, ls(below, all.names = TRUE))
        }
    }
    names
}

# Tests call student functions after the search path is swapped back, so their enclosure keeps what the student side attached.
.keep_student_attached <- function(student_parent) {
    kept <- new.env(parent = globalenv())
    env <- student_parent
    while (!.is_bottom_or_below(env, starting_parent_env)) {
        for (name in ls(env, all.names = TRUE)) {
            if (!exists(name, envir = kept, inherits = FALSE)) .bind_lazily(name, env, kept)
        }
        env <- parent.env(env)
    }
    parent.env(test_env$clean_env) <- kept
}

.bind_lazily <- function(name, from, to) {
    force(from)
    delayedAssign(name, get(name, envir = from, inherits = FALSE), assign.env = to)
}
