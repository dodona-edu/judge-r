.add_condition_message <- function(kind, what, condition) {
    get_reporter()$add_message(paste(kind, " while evaluating ", what, ": ", conditionMessage(condition), sep = ''))
}

.report_conditions <- function(what, expr) {
    withCallingHandlers(
        expr,
        warning = function(w) {
            .add_condition_message("Warning", what, w)
            invokeRestart("muffleWarning")
        },
        message = function(m) {
            if (!inherits(m, "packageStartupMessage")) {
                .add_condition_message("Message", what, m)
            }
            invokeRestart("muffleMessage")
        }
    )
}

# capture.output() swallows what is reported while it runs, so report its conditions once it returns.
.report_captured_conditions <- function(what, captured) {
    conditions <- list()
    on.exit(for (condition in conditions) {
        .add_condition_message(condition$kind, what, condition$condition)
    })
    withCallingHandlers(
        captured,
        warning = function(w) {
            conditions[[length(conditions) + 1]] <<- list(kind = "Warning", condition = w)
            invokeRestart("muffleWarning")
        },
        message = function(m) {
            if (!inherits(m, "packageStartupMessage")) {
                conditions[[length(conditions) + 1]] <<- list(kind = "Message", condition = m)
            }
            invokeRestart("muffleMessage")
        }
    )
}

.fail_testcase <- function(what, e) {
    .add_condition_message("Error", what, e)
    get_reporter()$escalate("runtime error")
    get_reporter()$end_testcase(accepted = FALSE)
}

.report_test_runtime_error <- function(description, e) {
    get_reporter()$start_test("", description)
    get_reporter()$end_test(conditionMessage(e), "runtime error")
}

# Expects the test started for `description` to still be open.
.close_test_with_runtime_error <- function(description, e) {
    get_reporter()$end_test("", "wrong")
    .report_test_runtime_error(description, e)
}

.report_image <- function(file, fail_if_absent, within_testcase) {
    if (file.exists(file)) {
        image <- base64enc::base64encode(file)
        get_reporter()$add_message(paste("<img src=\"data:image/png;base64,", image, "\"/>", sep=''), type = "html")
    } else if (fail_if_absent) {
        if (!within_testcase) {
            get_reporter()$start_testcase("")
        }
        get_reporter()$start_test("", "")
        get_reporter()$end_test("We expected an image, but it doesn't seem like the code generated one.", "wrong")
        if (!within_testcase) {
            get_reporter()$end_testcase()
        }
    } else {
        get_reporter()$add_message("We expected an image, but it doesn't seem like the code generated one.")
    }
}
