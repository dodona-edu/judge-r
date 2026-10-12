library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl))) + facet_wrap(~am)
p_labelled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "wt", y = "mpg")
p_scaled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + scale_x_log10()
p_faceted <- p
