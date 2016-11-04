
date()

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

fit.xf <- glm(cbind(y, N - y) ~ x + f, data = d, family = binomial)
fit.xf

options(repr.plot.width = 8)

d %>>% 
    mutate(pred = predict(fit.xf, newdata = data_frame(x = d$x, f = d$f), type = "response")) %>>% 
    ggplot() + 
        geom_point(aes(x = x, y = y)) +
        geom_line(aes(x = x, y = max(N) * pred)) + 
        facet_wrap(~f)

library(MASS)

stepAIC(fit.xf)

fit.xf.i <- glm(cbind(y, N - y) ~ x * f, family = binomial, data = d)
fit.xf.i

d %>>% 
    mutate(pred1 = predict(fit.xf, newdata = data_frame(x = d$x, f = d$f), type = "response"), 
          pred2 = predict(fit.xf.i, newdata = data_frame(x = d$x, f = d$f), type = "response")) %>>% 
    gather(model, pred, pred1:pred2) %>>% 
    ggplot(aes(x = x, y = max(N) * pred, group = f, colour = f)) + 
        geom_line() + 
        facet_wrap(~model)

d2 <- read_csv("data/chap06/data4b.csv")
str(d2)

options(repr.plot.width = 5)

ggplot(d2, aes(x = A, y = y)) + 
    geom_point(aes(colour = x)) + 
    scale_colour_gradient(low = "black", high = "white")

fit.offset <- glm(y ~ x, offset = log(A), family = poisson, data = d2)
fit.offset

paste0("pred.0", 0.1*10, collapse = "")

seq(0.1, 0.9, 0.2) %>>% lapply(function(x){
    d <- data_frame(x = x, A = d2$A)
    pred <- predict(fit.offset, newdata = d, type = "response")
    ret <- data_frame(pred)
    names(ret) <- as.character(x)
    ret
}) %>>% {bind_cols(d2["A"], (.))} %>>% 
    gather(x, pred, `0.1`:`0.9`, convert = TRUE) %>>% 
    ggplot() + 
        geom_point(data = d2, aes(x = A, y = y, colour = x)) + 
        geom_line(aes(x = A, y = pred, group = x, colour = x)) + 
        scale_colour_gradient(low = "black", high = gray(0.8)) + 
        theme_bw()

library(gridExtra)

options(repr.plot.width = 8, repr.plot.height = 8/3)

list(c(mean = 0, sd = 1), c(mean = 0, sd = 3), c(mean = 2, sd = 1)) %>>% 
    lapply(function(x){
        ggplot(data_frame(x = c(-5, 5)), aes(x)) + 
            stat_function(geom = "line", fun = dnorm, args = list(mean = x["mean"], sd = x["sd"])) + 
            theme_bw() + 
            geom_area(stat = "function", fun = dnorm, args = list(mean = x["mean"], sd = x["sd"]), 
                      fill = gray(0.5), xlim = c(1.2, 1.8)) + 
            scale_y_continuous(limit = c(0, 0.4))
    }) %>>% 
    {do.call(grid.arrange, c((.), list(ncol = 3)))}

pnorm(1.8, 0, 1) - pnorm(1.2, 0, 1)

dnorm(1.5, 0, 1) * (1.8 - 1.2)

list(c(r = 1), c(r = 5), c(r = 0.1)) %>>% 
    lapply(function(x){
        ggplot(data_frame(x = c(0, 5)), aes(x)) + 
            stat_function(geom = "line", fun = dgamma, args = list(shape = x["r"], rate = x["r"])) + 
            theme_bw() + 
            geom_area(stat = "function", fun = dgamma, args = list(shape = x["r"], rate = x["r"]), 
                      fill = gray(0.5), xlim = c(1.2, 1.8)) + 
            scale_y_continuous(limit = c(0, 1))
    }) %>>% 
    {do.call(grid.arrange, c((.), list(ncol = 3)))}

tmp = new.env()
load("data/chap06/d.RData", envir = tmp)
ls(tmp)

ls.str(tmp)

options(repr.plot.width = 4, repr.plot.height = 4)

tmp$d %>>% 
    ggplot(aes(x = x, y = y)) + 
        geom_point()

fit.gamma <- glm(y ~ log(x), family = Gamma(link = "log"), data = tmp$d)
fit.gamma

attributes(fit.gamma)

options(repr.plot.width = 4, repr.plot.height = 3)

ggplot(tmp$d, aes(x = x, y = y)) + 
    theme_bw() + 
    geom_point() + 
    stat_function(geom = "line", fun = function(x){exp(tmp$p[["b1"]] + tmp$p[["b2"]] * log(x))}, linetype = "dotted") + 
    geom_line(data = data_frame(x = tmp$d$x, 
                         pred = predict(fit.gamma, newdata = data_frame(x = tmp$d$x), type = "response")), 
              mapping = aes(x = x, y = pred)) + 
    
