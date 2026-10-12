root <- local_sandbox(teardown_env())
copy_judge(file.path(root, "judge"))

for (exercise in list.files(exercises_dir)) {
  submissions <- list.files(file.path(exercises_dir, exercise, "submissions"),
                            pattern = "\\.(R|Rmd)$", full.names = TRUE)
  for (submission in submissions) {
    case <- tools::file_path_sans_ext(basename(submission))
    test_that(paste0(exercise, "/", case), {
      check_case(root, exercise, case, submission)
    })
  }
}
