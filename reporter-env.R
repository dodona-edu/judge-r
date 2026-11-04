source("reporter-dodona.R", local=TRUE)

reporter_env <- new.env(parent = emptyenv())
reporter_env$reporter <- new_dodona_reporter()

get_reporter <- function() {
    reporter_env$reporter
}
