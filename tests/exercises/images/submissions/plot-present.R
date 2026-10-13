n <- 3
plot(1:n)
draw <- function() plot(1:n)
draw_broken <- function() stop("cannot draw")
