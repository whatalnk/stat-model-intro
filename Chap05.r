
sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

d <- read_csv("data/chap03/data3a.csv")
str(d)

fit.null <- glm(y ~ 1, data = d, family = "poisson")
fit.null

fit.x <- glm(y ~ x, data = d, family = "poisson")
fit.x

options(repr.plot.width = 4, repr.plot.height = 4)

ggplot(aes(x = x, y = y), data = d) + 
    geom_point() + 
    stat_smooth(formula = y ~ 1, method="glm", method.args = list(family = "poisson"), se = FALSE, size = 0.5, linetype = "dashed") + 
    stat_smooth(formula = y ~ x, method="glm", method.args = list(family = "poisson"), se = FALSE, size = 0.5) + 
    xlab(expression("Body size"~italic(x)[i])) + 
    ylab(expression("Number of seeds"~italic(y)[i]))


