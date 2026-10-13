repo_root <- normalizePath(test_path("..", ".."))
exercises_dir <- file.path(repo_root, "tests", "exercises")

# Copies the judge like Dodona's sync does: without what .dodonaignore lists.
# Patterns are anchored to the repo root, a trailing slash matches directories only.
copy_judge <- function(to) {
  lines <- trimws(readLines(file.path(repo_root, ".dodonaignore")))
  patterns <- lines[nzchar(lines) & !startsWith(lines, "#")]
  regexes <- glob2rx(c(patterns, paste0(patterns, "/")))
  is_ignored <- function(path) any(vapply(regexes, grepl, logical(1), x = path))

  copy_dir <- function(dir) {
    dir.create(file.path(to, dir), recursive = TRUE, showWarnings = FALSE)
    for (name in list.files(file.path(repo_root, dir), all.files = TRUE, no.. = TRUE)) {
      # Not tracked, so Dodona never syncs them.
      if (name %in% c(".git", ".claude")) next
      path <- file.path(dir, name)
      is_dir <- dir.exists(file.path(repo_root, path))
      if (is_ignored(if (is_dir) paste0(path, "/") else path)) next
      if (is_dir) copy_dir(path) else file.copy(file.path(repo_root, path), file.path(to, path))
    }
  }
  copy_dir("")
}

# JUDGE_R_IMAGE=1 runs the judge like Dodona does in the judge image: through
# /main.sh, in a sandbox under /mnt. Otherwise `run` is started directly, in a
# sandbox under the temporary directory.
in_judge_image <- nzchar(Sys.getenv("JUDGE_R_IMAGE"))

# Creates the sandbox that holds the judge, resources and submission for the whole run.
local_sandbox <- function(env = parent.frame()) {
  if (!in_judge_image) {
    return(withr::local_tempdir("sandbox", .local_envir = env))
  }
  if (!file.exists("/main.sh")) {
    stop("JUDGE_R_IMAGE is set, but /main.sh is missing: run the suite in the judge image")
  }
  if (file.access("/mnt", 2) != 0) {
    stop("/mnt is not writable: run the suite as root, like CI")
  }
  withr::local_tempdir(tmpdir = "/mnt", .local_envir = env)
}

# Runs the judge like Dodona does and returns the processx result.
run_judge <- function(root, exercise_dir, submission) {
  workdir <- withr::local_tempdir("workdir")
  dir.create(file.path(root, "submission"))
  on.exit(unlink(file.path(root, c("resources", "submission")), recursive = TRUE))

  dir.create(file.path(root, "resources"))
  file.copy(list.files(file.path(exercise_dir, "evaluation"), full.names = TRUE),
            file.path(root, "resources"), recursive = TRUE)
  file.copy(submission, file.path(root, "submission", "source"))

  config <- list(time_limit = 10, memory_limit = 256000000)
  config_file <- file.path(exercise_dir, "config.json")
  if (file.exists(config_file)) {
    config <- modifyList(config, jsonlite::fromJSON(config_file, simplifyVector = FALSE))
  }
  config <- c(config, list(
    programming_language = "R",
    natural_language = "en",
    judge = file.path(root, "judge"),
    resources = file.path(root, "resources"),
    source = file.path(root, "submission", "source"),
    workdir = workdir
  ))
  config_json <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(config, auto_unbox = TRUE, digits = NA), config_json)

  run <- file.path(root, "judge", "run")
  command <- if (in_judge_image) c("/main.sh", run) else run
  processx::run(command[1], command[-1], wd = workdir, stdin = config_json,
                timeout = 120, error_on_status = FALSE)
}

# Dodona's partial feedback format: each start-X is closed by close-X, and
# judgement > tab > context > testcase > test. Returns a description of the
# first violation, or NULL.
nesting_violation <- function(commands) {
  levels <- c("judgement", "tab", "context", "testcase", "test")
  open <- character()
  for (i in seq_along(commands)) {
    name <- commands[[i]]$command
    level <- sub("^(start|close)-", "", name)
    if (startsWith(name, "start-") && level %in% levels) {
      if (!identical(open, levels[seq_len(match(level, levels) - 1)])) {
        return(sprintf("command %d (%s) is not allowed inside [%s]", i, name, toString(open)))
      }
      open <- c(open, level)
    } else if (startsWith(name, "close-") && level %in% levels) {
      if (!identical(tail(open, 1), level)) {
        return(sprintf("command %d (%s) does not close the open [%s]", i, name, toString(open)))
      }
      open <- head(open, -1)
    } else if (!name %in% c("append-message", "escalate-status") || length(open) == 0) {
      return(sprintf("command %d (%s) is not allowed here", i, name))
    }
  }
  if (length(open) > 0) return(sprintf("[%s] never closed", toString(open)))
  if (length(commands) == 0 || commands[[length(commands)]]$command != "close-judgement") {
    return("the stream does not end with close-judgement")
  }
  NULL
}

png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))

# Replaces the payload of a PNG data URI by <png>; anything else stays visible.
hide_png <- function(text) {
  payload <- regexpr("(?<=data:image/png;base64,)[A-Za-z0-9+/=]+", text, perl = TRUE)
  if (payload > 0 &&
      identical(head(base64enc::base64decode(regmatches(text, payload)), 8), png_signature)) {
    regmatches(text, payload) <- "<png>"
  }
  text
}

hide_pngs <- function(feedback) rapply(feedback, hide_png, classes = "character", how = "replace")

to_jsonl <- function(commands) {
  vapply(commands, jsonlite::toJSON, "", auto_unbox = TRUE, digits = NA, null = "null")
}

unified_diff <- function(expected, actual) {
  files <- c(tempfile(), tempfile())
  on.exit(unlink(files))
  writeLines(expected, files[1], useBytes = TRUE)
  writeLines(actual, files[2], useBytes = TRUE)
  paste(system2("diff", c("-u", "--label", "expected", "--label", "actual", files),
                stdout = TRUE), collapse = "\n")
}

check_case <- function(root, exercise, case, submission) {
  exercise_dir <- file.path(exercises_dir, exercise)
  expected_file <- file.path(exercise_dir, "expected", paste0(case, ".jsonl"))

  result <- run_judge(root, exercise_dir, submission)
  stderr <- paste0("\nstderr:\n", result$stderr)
  if (!identical(result$status, 0L)) {
    fail(sprintf("judge exited with status %s%s", format(result$status), stderr))
  }

  stdout <- strsplit(result$stdout, "\n")[[1]]
  Encoding(stdout) <- "UTF-8"
  commands <- tryCatch(
    lapply(stdout, jsonlite::fromJSON, simplifyVector = FALSE),
    error = function(e) fail(sprintf("stdout is not one JSON command per line: %s%s", conditionMessage(e), stderr))
  )
  violation <- nesting_violation(commands)
  if (!is.null(violation)) fail(paste0(violation, stderr))

  actual <- to_jsonl(hide_pngs(commands))
  if (nzchar(Sys.getenv("UPDATE_EXPECTED"))) {
    if (!in_judge_image) {
      stop("UPDATE_EXPECTED records the image's feedback: run it through devel/run-tests-in-image.sh")
    }
    dir.create(dirname(expected_file), showWarnings = FALSE)
    writeLines(actual, expected_file, useBytes = TRUE)
  } else {
    if (!file.exists(expected_file)) {
      fail(sprintf("%s is missing: run with UPDATE_EXPECTED=1 to create it", expected_file))
    }
    expected <- readLines(expected_file, encoding = "UTF-8", warn = FALSE)
    if (!identical(expected, actual)) {
      fail(paste0("feedback differs from ", expected_file, "\n", unified_diff(expected, actual), stderr))
    }
  }
  succeed()
}
