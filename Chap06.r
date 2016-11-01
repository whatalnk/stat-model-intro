
library(readr)

library(pipeR)

d <- readr::read_csv("data/chap06/data4a.csv")
str(d)

library(dplyr)

d %>>% mutate(f = as.factor(f)) -> d
summary(d)

library(ggplot2)
library(tidyr)

options(repr.plot.width = 4, repr.plot.height = 4)

d %>>% ggplot(aes(x = x, y = y, group = f, colour = f)) + 
    geom_point() + 
    theme(
        legend.position = c(.1, .85)
    )

ggplot(data_frame(x = c(0:8)), aes(x)) + 
    stat_function(geom="line", n=9, fun = dbinom, args = list(size = 8, prob = 0.1), colour = "blue") +
    stat_function(geom="point", n=9, fun = dbinom, args = list(size = 8, prob = 0.1), colour = "blue") +
    stat_function(geom="line", n=9, fun = dbinom, args = list(size = 8, prob = 0.3), colour = "green") + 
    stat_function(geom="point", n=9, fun = dbinom, args = list(size = 8, prob = 0.3), colour = "green") + 
    stat_function(geom="line", n=9, fun = dbinom, args = list(size = 8, prob = 0.8), colour = "red") + 
    stat_function(geom="point", n=9, fun = dbinom, args = list(size = 8, prob = 0.8), colour = "red")

ggplot(data_frame(x = c(-6:6)), aes(x)) + 
    stat_function(geom = "line", fun = function(z){1 / (1 + exp(-z))}) + 
    geom_vline(xintercept = 0, linetype = "dotted")

logistic <- function(x, beta1, beta2){1 / (1 + exp(-(beta1 + beta2 * x)))}
ggplot(data_frame(x = c(-3:3)), aes(x)) + 
    stat_function(geom = "line", fun = logistic, args = list(beta1 = 0, beta2 = 2)) + 
    stat_function(geom = "line", fun = logistic, args = list(beta1 = 2, beta2 = 2), colour = gray(0.5)) + 
    stat_function(geom = "line", fun = logistic, args = list(beta1 = -3, beta2 = 2), colour = gray(0.5)) + 
    geom_vline(xintercept = 0, linetype = "dotted") + 
    annotate(geom = "text", label = "beta[1]==0", x = 0.5, y = 0.5, parse = TRUE) + 
    annotate(geom = "text", label = "beta[1]==2", x = -2, y = 0.8, parse = TRUE) + 
    annotate(geom = "text", label = "beta[1]==-3", x = 2, y = 0.4, parse = TRUE)

ggplot(data_frame(x = c(-3:3)), aes(x)) + 
    stat_function(geom = "line", fun = logistic, args = list(beta1 = 0, beta2 = 2)) + 
    stat_function(geom = "line", fun = logistic, args = list(beta1 = 0, beta2 = 4), colour = gray(0.5)) + 
    stat_function(geom = "line", fun = logistic, args = list(beta1 = 0, beta2 = -1), colour = gray(0.5)) + 
    geom_vline(xintercept = 0, linetype = "dotted") + 
    annotate(geom = "text", label = "beta[2]==2", x = -2, y = 0.25, parse = TRUE) + 
    annotate(geom = "text", label = "beta[2]==4", x = -0.3, y = 0.95, parse = TRUE) + 
    annotate(geom = "text", label = "beta[2]==-1", x = 2, y = 0.3, parse = TRUE)
