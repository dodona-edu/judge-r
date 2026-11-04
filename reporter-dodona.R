# One JSON command per line on stdout; NULL fields are left out (toJSON would write them as {}).
.write_command <- function(command, ...) {
    fields <- Filter(Negate(is.null), list(command = command, ...))
    write(jsonlite::toJSON(fields, auto_unbox = TRUE), stdout())
}

new_dodona_reporter <- function() {
    structure(list(
        start_reporter = function() .write_command("start-judgement"),
        start_tab = function(name) .write_command("start-tab", title = name),
        start_context = function() .write_command("start-context"),
        start_testcase = function(description) .write_command("start-testcase", description = description),
        start_test = function(expected, description) {
            .write_command("start-test", expected = toString(expected), description = toString(description))
        },
        end_test = function(generated, status) {
            .write_command("close-test", generated = toString(generated), status = list(enum = status))
        },
        end_testcase = function(accepted = NULL) .write_command("close-testcase", accepted = accepted),
        end_context = function(accepted = NULL) .write_command("close-context", accepted = accepted),
        end_tab = function() .write_command("close-tab"),
        end_reporter = function() .write_command("close-judgement"),
        add_message = function(message, type = "plain") {
            .write_command("append-message", message = list(description = toString(message), format = type))
        },
        escalate = function(status) .write_command("escalate-status", status = list(enum = status))
    ), class = "DodonaReporter")
}
