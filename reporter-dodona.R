DodonaReporter <- R6::R6Class("DodonaReporter",
  public = list(
    start_reporter = function() {
        write('{"command": "start-judgement"}', stdout())
    },

    start_tab = function(name) {
        write(paste('{"command": "start-tab", "title": ', toJSON(name, auto_unbox=TRUE), '}', sep=''), stdout())
    },

    start_context = function(context) {
        write(paste('{"command": "start-context", "description": ', toJSON(context, auto_unbox=TRUE), '}', sep=''), stdout())
    },

    start_testcase = function(description) {
        write(paste('{"command": "start-testcase", "description": ', toJSON(description, auto_unbox=TRUE), '}', sep=''), stdout())
    },

    start_test = function(expected, description) {
        write(paste('{"command": "start-test", "expected": ', toJSON(toString(expected), auto_unbox=TRUE) ,', "description": ', toJSON(toString(description), auto_unbox=TRUE), '}', sep=''), stdout())
    },

    end_test = function(generated, status) {
        write(paste('{"command": "close-test", "generated": ', toJSON(toString(generated), auto_unbox=TRUE) ,', "status": {"enum": "', status, '"}}', sep=''), stdout())
    },

    end_testcase = function() {
        write('{"command": "close-testcase"}', stdout())
    },

    end_context = function() {
        write('{"command": "close-context"}', stdout())
    },

    end_tab = function() {
        write('{"command": "close-tab"}', stdout())
    },

    end_reporter = function() {
        write('{"command": "close-judgement"}', stdout())
    },

    add_message = function(message) {
        write(paste('{"command": "append-message", "message": ', toJSON(toString(message), auto_unbox=TRUE), '}', sep=''), stdout())
    },

    escalate = function(status) {
        write(paste('{"command": "escalate-status", "status": {"enum": "', status, '"}}', sep=''), stdout())
    }
  )
)
