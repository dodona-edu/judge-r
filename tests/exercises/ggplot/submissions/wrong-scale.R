library(ggplot2)
p <- ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour = factor(cyl))) + facet_wrap(~am)
p_labelled <- ggplot(mtcars, aes(wt, mpg)) + geom_point() + labs(x = "Weight", y = "Miles per gallon")
p_scaled <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
p_faceted <- p
