# help() and file.show() otherwise page through an external program, which writes past capture.output().
options(pager = function(files, header, title, delete.file) {
    writeLines(unlist(lapply(files, readLines)))
    if (delete.file) unlink(files)
})

source('reporter-env.R', local=TRUE)
source('feedback.R', local=TRUE)
source('context.R', local=TRUE)
source('testcase.R', local=TRUE)
source('test.R', local=TRUE)
source('utils-ast.R', local=TRUE)
source('utils-plot.R', local=TRUE)
source('utils-df.R', local=TRUE)

student_code <- NULL

# In the global environment, so the student code and the evaluation files find it before utils.
assign("install.packages", function(...) {
    warning("install.packages() is not available on Dodona; installed packages can be loaded with library()", call. = FALSE)
    invisible(NULL)
}, envir = globalenv())

start_judgement <- function(tests_path, source_path) {
    get_reporter()$start_reporter()
    on.exit(get_reporter()$end_reporter())

    # We need the source path when constructing clean contexts
    student_code <<- source_path

    test_files = list.files(tests_path)
    for (file in test_files) {
        start_tab(paste(tests_path, file, sep = '/'), file)
    }
}

strip_extension <- function(filename) {
    gsub("\\.[Rr]$", "", filename)
}

strip_leading_digit <- function(filename) {
    gsub("^\\d*-", "", filename)
}

start_tab <- function(path, filename) {
    get_reporter()$start_tab(strip_leading_digit(strip_extension(filename)))
    on.exit(get_reporter()$end_tab())

    source(path, local=TRUE)
}
