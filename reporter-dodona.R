DodonaReporter <- R6::R6Class("DodonaReporter",
  public = list(
    start_reporter = function() {
        write('{"command": "start-judgement"}', stdout())
    },

    start_tab = function(name) {
        write(paste('{"command": "start-tab", "title": ', jsonlite::toJSON(name, auto_unbox=TRUE), '}', sep=''), stdout())
    },

    start_context = function() {
        write('{"command": "start-context" }', stdout())
    },

    start_testcase = function(description) {
        write(paste('{"command": "start-testcase", "description": ', jsonlite::toJSON(description, auto_unbox=TRUE), '}', sep=''), stdout())
    },

    start_test = function(expected, description) {
        write(paste('{"command": "start-test", "expected": ', jsonlite::toJSON(toString(expected), auto_unbox=TRUE) ,', "description": ', jsonlite::toJSON(toString(description), auto_unbox=TRUE), '}', sep=''), stdout())
    },

    end_test = function(generated, status) {
        write(paste('{"command": "close-test", "generated": ', jsonlite::toJSON(toString(generated), auto_unbox=TRUE) ,', "status": {"enum": "', status, '"}}', sep=''), stdout())
    },

    end_testcase = function(accepted = NULL) {
        if(is.null(accepted)) {
            write('{"command": "close-testcase"}', stdout())
        } else {
            write(paste('{"command": "close-testcase", "accepted": ', jsonlite::toJSON(accepted, auto_unbox=TRUE) ,'}', sep=''), stdout())
        }
    },

    end_context = function(accepted = NULL) {
        if(is.null(accepted)) {
            write('{"command": "close-context"}', stdout())
        } else {
            write(paste('{"command": "close-context", "accepted": ', jsonlite::toJSON(accepted, auto_unbox=TRUE) ,'}', sep=''), stdout())
        }
    },

    end_tab = function() {
        write('{"command": "close-tab"}', stdout())
    },

    end_reporter = function() {
        write('{"command": "close-judgement"}', stdout())
    },

    add_message = function(message, type="plain") {
        write(paste('{"command": "append-message", "message": { "description": ', jsonlite::toJSON(toString(message), auto_unbox=TRUE), ', "format": ', jsonlite::toJSON(type, auto_unbox=TRUE), '} }', sep=''), stdout())
    },

    escalate = function(status) {
        write(paste('{"command": "escalate-status", "status": {"enum": "', status, '"}}', sep=''), stdout())
    }
  )
)
