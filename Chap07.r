
date()

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

d <- readr::read_csv("data/chap07/data.csv")
str(d)

options(repr.plot.width = 4, repr.plot.height = 4)

d %>>% ggplot(aes(x = x, y = y)) + 
    geom_point(shape = 21, position = position_jitter(w = 0.1, h = 0))

fit.glm <- glm(cbind(y, N - y) ~ x, data = d, family = binomial)
summary(fit.glm)

logistic <- function(x, beta1, beta2){
    1 / (1 + exp(-(beta1 + beta2 * x)))
}

d %>>% 
    mutate(pred.true = logistic(d$x, -4, 1), 
           pred = predict(fit.glm, newdata = data.frame(x = d$x), type = "response")) %>>% 
    ggplot() + 
    geom_point(aes(x = x, y = y), shape = 21, position = position_jitter(w = 0.1, h = 0)) + 
    geom_line(aes(x = x, y = 8 * pred)) + 
    geom_line(aes(x = x, y = 8 * pred.true), linetype = "dotted")

d %>>% filter(x == 4) %>>% (y) %>>% table()

d %>>% filter(x == 4) %>>% 
    {ggplot(data = (.), aes(x = y)) + 
    stat_count(geom = "point", shape = 21) + 
    stat_function(geom="point", n=9, fun = function(x, size, prob){dbinom(x, size = size, prob = prob) * nrow(.)}, 
                  args = list(size = 8, prob = logistic(4, -2.15, 0.51))) + 
    stat_function(geom="line", n=9, fun = function(x, size, prob){dbinom(x, size = size, prob = prob) * nrow(.)}, 
                  args = list(size = 8, prob = logistic(4, -2.15, 0.51)))}

d %>>% filter(x == 4) %>>% summarize_each(funs(mean, var), y)

4.05 / 8

logistic(4, -2.15, 0.51) %>>% {8 * (.) * (1 - (.))}

library(glmmML)

fit.glmm <- glmmML(cbind(y, N - y) ~ x, data = d, family = binomial, cluster = id, method = "ghq")
fit.glmm

fit.glmm2 <- glmmML(cbind(y, N - y) ~ x, data = d, family = binomial, cluster = id)
fit.glmm2

data_frame(xx = seq(1.6, 6.4, 0.1)) %>>% 
    mutate(pred.true = logistic(xx, -4, 1), 
           pred = logistic(xx, fit.glmm$coefficients[1], fit.glmm$coefficients[2])) %>>% 
    ggplot() + 
    geom_point(mapping = aes(x = x, y = y), data = d, shape = 21, position = position_jitter(w = 0.1, h = 0)) + 
    geom_line(aes(x = xx, y = 8 * pred)) + 
    geom_line(aes(x = xx, y = 8 * pred.true), linetype = "dotted")
