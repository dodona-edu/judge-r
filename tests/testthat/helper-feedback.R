# The command names in feedback from capture_feedback().
feedback_commands <- function(feedback) {
  vapply(feedback, function(command) command$command, character(1))
}

# The descriptions of the append-message commands in feedback from capture_feedback().
feedback_messages <- function(feedback) {
  messages <- Filter(function(command) command$command == "append-message", feedback)
  vapply(messages, function(command) command$message$description, character(1))
}

# A child of `judge` that holds `...`: pass it to capture_feedback() to use values
# from the test in the evaluation code, which cannot see the test's variables.
with_values <- function(judge, ...) {
  list2env(list(...), parent = judge)
}
