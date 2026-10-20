# What `run` attaches before it sources judge.R; judge code calls them unqualified.
judge_packages <- c("jsonlite", "base64enc", "R6", "rlang", "purrr")

# `run` saves this before attaching anything. Here testthat is attached already,
# so student code in a context can see testthat; it cannot see judge_packages.
judge_starting_parent_env <- parent.env(.GlobalEnv)

# Loads the judge into a fresh environment, like `run` does, and returns that
# environment: every call has its own reporter, test_env and student_code.
local_judge <- function(env = parent.frame()) {
  for (package in judge_packages) {
    suppressPackageStartupMessages(library(package, character.only = TRUE))
  }

  # A context swaps the parent of the global environment while it evaluates student code.
  global_parent <- parent.env(.GlobalEnv)
  withr::defer(`parent.env<-`(.GlobalEnv, global_parent), envir = env)
  # judge.R assigns install.packages in the global environment.
  withr::defer(suppressWarnings(rm("install.packages", envir = .GlobalEnv)), envir = env)

  judge <- new.env(parent = globalenv())
  judge$starting_parent_env <- judge_starting_parent_env
  source(file.path(test_path("..", ".."), "judge.R"), local = judge, chdir = TRUE)
  judge
}

# Points the judge at a submission containing `code`, like start_judgement does.
local_student_code <- function(judge, code, env = parent.frame()) {
  path <- withr::local_tempfile(lines = code, fileext = ".R", .local_envir = env)
  judge$student_code <- path
  invisible(path)
}

# Evaluates `code` the way an evaluation file runs (in a child of the judge
# environment) and returns what the reporter wrote: one parsed command per line.
capture_feedback <- function(judge, code) {
  code <- substitute(code)
  # Like source(), don't print the value of the code.
  lines <- utils::capture.output(invisible(eval(code, new.env(parent = judge))))
  lapply(lines, function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
      error = function(e) stop("Not a feedback command: ", line, call. = FALSE)
    )
  })
}
