testcase <- function(description = "", tests={}) {
    get_reporter()$start_testcase(description)
    do_exit <- TRUE
    on.exit({
        if(do_exit) {
            get_reporter()$end_testcase()
        }
    })

    tryCatch({ eval(tests) }, error = function(e) {
        .fail_testcase("testcase", e)
        do_exit <<- FALSE
    })
}

testcaseAssert <- function(description, checker) {
    get_reporter()$start_testcase(description)
    tryCatch(
        .report_conditions("assert", {
            .report_captured_conditions("assert", capture.output(checker_val <- .with_student_jit(checker(test_env$clean_env))))
            if (!checker_val) {
                get_reporter()$escalate("wrong")
            }
            get_reporter()$end_testcase(accepted = checker_val)
        }),
        error = function(e) {
            .fail_testcase("assert", e)
        }
    )
}
