
date()

sapply(c("pipeR", "ggplot2", "dplyr", "tidyr", "readr"), require, character.only = TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)

d <- readr::read_csv("data/chap03/data3a.csv")

str(d)

summary(d)

d %>>% dplyr::mutate(f = as.factor(f)) -> d
summary(d)

str(d)

class(d)

d %>>% 
    ggplot(aes(x = x, y = y, shape = f)) + 
    geom_point() + 
    scale_shape_manual(values = c(21, 19)) + 
    theme(legend.position=c(.1, .85))

d %>>% 
    ggplot(aes(x = f, y = y)) + 
    geom_boxplot()

ggplot(data_frame(x = c(-4, 4)), aes(x)) + 
    stat_function(fun = function(x, beta1, beta2){exp(-1 + 0.4 * x)}) + 
    annotate("text", x = -3, y = 3, label = 'atop("{"*beta[1]*", "*beta[2]*"}      ", " = {-2, -0.8}")', parse = TRUE, hjust = 0, vjust = 0) + 
    stat_function(fun = function(x, beta1, beta2){exp(-2 + -0.8 * x)}, linetype = 2) +
    annotate("text", x = 2, y = 2, label = 'atop("{"*beta[1]*", "*beta[2]*"}      ", " = {-1, 0.4}")', parse = TRUE, hjust = 0, vjust = 0)

fit <- glm(y ~ x, data = d, family = poisson(link = "log"))

fit

summary(fit)

logLik(fit)

fit$coefficients[["(Intercept)"]]

xx <- seq(min(d$x), max(d$x), length = 100)
d %>>% 
    mutate(xx = xx, fitted = exp(fit$coefficients[["(Intercept)"]] + fit$coefficients[["x"]] * xx)) %>>%
    ggplot(aes(x = x, y = y, shape = f)) + 
        geom_point() + 
        geom_line(aes(x = xx, y = fitted), inherit.aes = FALSE) + 
        scale_shape_manual(values = c(21, 19)) + 
        theme(legend.position=c(.1, .85))

fit.f <- glm(y ~ f, data = d, family = poisson(link = "log"))

fit.f

summary(fit.f)

exp(fit.f$coefficients[["(Intercept)"]])

exp(fit.f$coefficients[["(Intercept)"]] + fit.f$coefficients[["fT"]])

logLik(fit.f)

fit.all <- glm(y ~ x + f, data = d, family = poisson(link = "log"))

fit.all

summary(fit.all)

logLik(fit.all)

devtools::session_info()
