ordered_scores <- data.frame(id = 1:8,
                             name = c("ada", "bob", "cleo", "dan", "eve", "fay", "gus", "hal"),
                             score = c(9.5, 7, 8.25, 6, 10, 5.5, 7.75, 8))
scores <- ordered_scores[c(5, 2, 8, 1, 3, 7, 4, 6), c("score", "id", "name")]
