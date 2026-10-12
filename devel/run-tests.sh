#!/usr/bin/env bash
# Runs the test suite with the R on the PATH; an optional argument filters the test files by name.
set -euo pipefail
cd "$(dirname "$0")/.."
Rscript -e 'library(testthat)
args <- commandArgs(TRUE)
reporter <- MultiReporter$new(list(LocationReporter$new(), SummaryReporter$new(show_praise = FALSE)))
test_dir("tests/testthat", filter = if (length(args)) args[1], reporter = reporter,
         stop_on_failure = TRUE)' "$@"
