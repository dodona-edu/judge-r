# An htest object like t.test() returns, with round numbers.
new_htest <- function(p.value = 0.04, conf.int = c(1, 2), conf.level = 0.95,
                      statistic = c(t = 2.5), alternative = "two.sided",
                      method = "Welch Two Sample t-test") {
  structure(list(
    statistic = statistic,
    p.value = p.value,
    conf.int = structure(conf.int, conf.level = conf.level),
    alternative = alternative,
    method = method
  ), class = "htest")
}

# A judge whose clean_env holds `...`.
local_judge_with <- function(..., env = parent.frame()) {
  judge <- local_judge(env)
  judge$test_env$clean_env <- list2env(list(...))
  judge
}

tempdir_pngs <- function() {
  list.files(tempdir(), pattern = "\\.png$", full.names = TRUE)
}

# Removes the PNG files the calling test leaves in tempdir().
local_tempdir_pngs <- function(env = parent.frame()) {
  before <- tempdir_pngs()
  withr::defer(unlink(setdiff(tempdir_pngs(), before)), envir = env)
}
