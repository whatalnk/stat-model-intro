
date()

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

d <- read_csv("data/chap03/data3a.csv")
str(d)

d %>>% mutate(f = factor(f)) -> d

glm(y ~ 1, data = d, family = "poisson")

glm(y ~ f, data = d, family = "poisson")

glm(y ~ x, data = d, family = "poisson")

glm(y ~ x + f, data = d, family = "poisson")

dpois(d$y, lambda = d$y) %>>% log %>>% sum

dpois(d$y, lambda = d$y) %>>% log %>>% sum %>>% {(.) * (-2)} -> D.full
D.full

glm(y ~ 1, data = d, family = "poisson") -> fit.c
logLik(fit.c)

-2 * -237.6432 - D.full

fit.c$deviance

ls()

load("data/chap04/data.RData", dum <- new.env())
ls(dum)

dum$m.data %>>% str()

dum$m.data[,1]

devtools::session_info()
