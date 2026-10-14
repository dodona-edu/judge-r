#!/usr/bin/env Rscript
# Compares renv.lock with the R and packages installed where it runs: in CI, the judge image.

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
lock <- jsonlite::fromJSON(file.path(dirname(dirname(normalizePath(script))), "renv.lock"),
                           simplifyVector = FALSE)
locked <- vapply(lock$Packages, function(record) record$Version, "")

# installed.packages() lists the libraries in .libPaths() order, so the first row is the one R loads.
installed <- installed.packages()
installed <- installed[!duplicated(installed[, "Package"]), , drop = FALSE]
image <- unname(installed[match(names(locked), installed[, "Package"]), "Version"])
r_version <- paste(R.version$major, R.version$minor, sep = ".")

rows <- data.frame(
  name = c("R", names(locked)),
  locked = c(lock$R$Version, unname(locked)),
  image = c(r_version, ifelse(is.na(image), "not installed", image))
)
differing <- rows[rows$locked != rows$image, ]

report <- function(...) {
  text <- paste0(..., "\n")
  cat(text)
  summary <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(summary)) cat(text, file = summary, append = TRUE)
}

if (nrow(differing) == 0) {
  report(sprintf("renv.lock matches the image: R %s and all %d locked packages.",
                 r_version, length(locked)))
  quit(status = 0)
}

report("renv.lock does not match the R and packages installed in the image. Either this branch ",
       "proposes versions the image does not ship yet, in which case the PR becomes mergeable once ",
       "docker-images has published them; or master's renv.lock went stale after an image publish, ",
       "in which case regenerate it with the command in the README's Tests section.")
report()
report("| Package | renv.lock | image |")
report("|---|---|---|")
report(paste(sprintf("| %s | %s | %s |", differing$name, differing$locked, differing$image),
             collapse = "\n"))
quit(status = 1)
