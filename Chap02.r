
date()

sapply(c("pipeR", "ggplot2", "dplyr", "tidyr", "readr"), require, character.only = TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)

dir(".", full.names = TRUE) %>>% cat(sep = "\n")

dir("./data", full.names = TRUE, recursive = TRUE) %>>% cat(sep = "\n")

ls.str()

load("data/chap02/data.RData")

ls.str()

data

length(data)

summary(data)

table(data)

data_frame(data = data) %>>% 
    ggplot(aes(x = data)) + 
        geom_histogram(binwidth = 1, colour = "black", fill = gray(0.6)) + 
        scale_y_continuous(breaks = seq(0, 12, 2)) + 
        scale_x_continuous(breaks = seq(0, 8, 2), limits = c(-0.5, 8.1)) + 
        labs(title = "Histogram of data") + 
        ylab("Frequency")

var(data)

sd(data)

var(data) %>>% sqrt

y <- 0:9
prob <- dpois(y, lambda = 3.56)

cbind(y, prob)

data_frame(y, prob) %>>% 
    ggplot(aes(y, prob)) + 
    geom_line(linetype = 2) +
    geom_point() + 
    scale_x_continuous(breaks = seq(0, 8, 2))

ggplot() + 
    geom_histogram(data = data_frame(x = data), aes(x = x), 
                   colour = "black", fill = gray(0.6), binwidth = 1) + 
    geom_line(data = data_frame(y, prob = prob * 50), aes(x = y, y = prob), linetype = 2) + 
    geom_point(data = data_frame(y, prob = prob * 50), aes(x = y, y = prob), 
               shape = 21, fill = "white") + 
    labs(title = "Histogram of data") + 
    ylab("Frequency") + 
    xlab("data") + 
    scale_x_continuous(breaks = seq(0, 8, 2)) + 
    scale_y_continuous(breaks = seq(0, 12, 2))

data_frame(x = c(0:20), y1 = dpois(x, lambda = 3.5), y2 = dpois(x, lambda = 7.7), 
           y3 = dpois(x, lambda = 15.1)) %>>%
    gather(lambda, val, -x) %>>%
    ggplot(aes(x = x, y = val, shape = lambda)) + 
    geom_line() + 
    geom_point(fill = "white") + 
    ylab("prob") + 
    xlab("y") + 
    scale_shape_manual(values = c(21, 23, 24), labels = c("3.5", "7.7", "15.1")) + 
    theme(legend.position=c(.9, .85))

data[1:3]

dpois(data[1:3], lambda = 3.56)

dpois(data[1:3], lambda = 3.56) %>>% prod()

logL <- function(m){
    dpois(data, m, log = TRUE) %>>% sum
}

gpPois <- function(lambda){
    gp <- ggplot() + 
    geom_histogram(data = data_frame(x = data), aes(x = x), 
                   colour = "black", fill = gray(0.6), binwidth = 1) + 
    geom_line(data = data_frame(y, prob = dpois(y, lambda = lambda) * 50), aes(x = y, y = prob), 
              linetype = 2) + 
    geom_point(data = data_frame(y, prob = dpois(y, lambda = lambda) * 50), aes(x = y, y = prob), 
               shape = 21, fill = "white") + 
    scale_x_continuous(breaks = seq(0, 8, 2)) + 
    scale_y_continuous(breaks = seq(0, 15, 5), limits = c(0, 15)) + 
    annotate(x = 6, y = 13, hjust = 0, vjust = 0, geom = "text", size = 3, 
             label = sprintf("lambda=%.1f\nlog L=%.1f", lambda, logL(lambda))) + 
    theme(axis.title = element_blank())
    return(gp)
}

gps <- seq(2.0, 5.2, 0.4) %>>% lapply(gpPois)

library(gridExtra)

options(repr.plot.width = 8, repr.plot.height = 8)

do.call(grid.arrange, c(gps, ncol = 3))

options(repr.plot.width = 4, repr.plot.height = 4)

lambda = seq(2, 5, 0.1)
data_frame(lambda, logL = sapply(lambda, logL)) %>>% 
    ggplot(aes(x = lambda, y = logL)) + 
        geom_line() + 
        geom_vline(xintercept = 3.56, linetype = 2) + 
        scale_x_continuous(breaks = seq(2, 5, 0.5))

binom.test(4, 10)

logL <- function(t){
    4 * log(t) + 6 * log(1 - t)
}
ggplot(data_frame(x = c(0, 1)), aes(x)) + 
    stat_function(fun = logL) + 
    xlab(expression(theta)) + 
    ylab(expression("L("~theta~")")) + 
    geom_vline(xintercept = c(0.12, 0.74), linetype = 2)

data_frame(y = sapply(c(1:3000), function(x){
    rpois(50, lambda = 3.5) %>>% mean
})) %>>% ggplot(aes(x = y)) + 
    geom_histogram(colour = "black", fill = gray(0.6), binwidth = 0.1) + 
    scale_x_continuous(limits = c(2.5, 4.5), breaks = seq(2.5, 4.5, 0.5)) + 
    xlab(expression("Estimated "~lambda~"for each trial"))

data_frame(y = sapply(c(1:3000), function(x){
    rpois(500, lambda = 3.5) %>>% mean
})) %>>% ggplot(aes(x = y)) + 
    geom_histogram(colour = "black", fill = gray(0.6), binwidth = 0.1) + 
    scale_x_continuous(limits = c(2.5, 4.5), breaks = seq(2.5, 4.5, 0.5)) + 
    xlab(expression("Estimated "~lambda~"for each trial"))

ggplot(data_frame(x = c(0:40)), aes(x)) + 
    stat_function(geom="point", n=41, fun = dbinom, args = list(size = 10, prob = 0.4), colour = "blue") +
    stat_function(geom="point", n=41, fun = dbinom, args = list(size = 20, prob = 0.7), colour = "green") + 
    stat_function(geom="point", n=41, fun = dbinom, args = list(size = 40, prob = 0.5), colour = "red")

ggplot(data_frame(x = c(0, 20)), aes(x)) + 
       stat_function(fun = dgamma, args = list(shape = 1, rate = 0.5), colour = "red") + 
       stat_function(fun = dgamma, args = list(shape = 2, rate = 0.5), colour = "orange") + 
       stat_function(fun = dgamma, args = list(shape = 1, rate = 1), colour = "blue") + 
       stat_function(fun = dgamma, args = list(shape = 2, rate = 1), colour = "green") 

devtools::session_info()
