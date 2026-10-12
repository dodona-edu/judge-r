library(ggplot2)
p <- ggplot(cars, aes(speed, dist)) + geom_point()

p_labelled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "Miles per gallon")
p_scaled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10()
p_faceted <- p
