
if (interactive()) {
    (function(x){
        notinstalled <- x[!(x %in% installed.packages()[, "Package"])]
        if (length(notinstalled) > 0) {
            cat(sprintf("Install %d packages: %s \n", length(notinstalled), notinstalled))
            install.packages(notinstalled)
        } else {
            cat("Already installed\n")
           installed.packages()[, "Version"][x] 
        }
    })(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr", "knitr", "devtools"))
}

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

d <- read_csv("data/chap03/data3a.csv")
str(d)

fit.null <- glm(y ~ 1, data = d, family = "poisson")
fit.null

logLik(fit.null)

fit.x <- glm(y ~ x, data = d, family = "poisson")
fit.x

logLik(fit.x)

fit.full <- list()
fit.full$maxLogLik <- dpois(d$y, lambda = d$y) %>>% log() %>>% sum()
fit.full$dev <- -2 * fit.full$maxLogLik
fit.full$aic <- -2 * (fit.full$maxLogLik - length(d$y))
fit.full

options(repr.plot.width = 4, repr.plot.height = 4)

ggplot(aes(x = x, y = y), data = d) + 
    geom_point() + 
    stat_smooth(formula = y ~ 1, method="glm", method.args = list(family = "poisson"), se = FALSE, size = 0.5, linetype = "dashed") + 
    stat_smooth(formula = y ~ x, method="glm", method.args = list(family = "poisson"), se = FALSE, size = 0.5) + 
    xlab(expression("Body size"~italic(x)[i])) + 
    ylab(expression("Number of seeds"~italic(y)[i]))

data.frame(
    "model" = c("Null", "x", "Full"), 
    "k" = c(1, 2, 100), 
    "max.log.L" = c(logLik(fit.null), logLik(fit.x), fit.full$maxLogLik), 
    "deviance" = c(fit.null$deviance, fit.x$deviance, 0) + fit.full$dev, 
    "resd.dev." = c(fit.null$deviance, fit.x$deviance, 0), 
    "aic" = c(fit.null$aic, fit.x$aic, fit.full$aic)
) %>>% mutate_each(funs(round(., 1)), max.log.L:aic) %>>% knitr::kable()

(fit.null$deviance - fit.x$deviance)

readLines("Data/chap05/pbnew.R", encoding = "UTF-8") %>>% cat(sep="\n")

readLines("Data/chap05/pb.R", encoding = "UTF-8") %>>% cat(sep="\n")

source("Data/chap05/pb.R")
# or
# source("http://hosho.ees.hokudai.ac.jp/~kubo/stat/iwanamibook/fig/lrtest/pb.R")
pb

dd12 <- pb(d, n.bootstrap = 1000)

summary(dd12)

options(repr.plot.width = 4, repr.plot.height = 4)

data.frame(x = dd12) %>>% 
    ggplot(aes(x = x)) + 
    geom_histogram(bins = 100, colour = "black", fill = "white", size = 0.3) + 
    scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) + 
    theme_bw() + 
    geom_vline(xintercept = 4.5, size = 0.5, linetype = "dotted") + 
    xlab(expression(Delta~italic(D)[`1,2`]))

sum(dd12 >= 4.5)

as.numeric(c(TRUE, FALSE))

sum(dd12 >= 4.5) / 1000

quantile(dd12, 0.95)

library(boot)

b <- boot(
    d, 
    function(data){
        fit1 <- glm(y ~ 1, data = data, family = poisson)
        fit2 <- glm(y ~ x, data = data, family = poisson)
        fit1$deviance - fit2$deviance
    }, 
    R = 1000, 
    sim = "parametric", 
    ran.gen = function(data, mle){
        out <- data
        out$y <- rpois(nrow(out), lambda = mle)
        out
    }, 
    mle = mean(d$y)
)
b

str(b)

summary(b$t[,1])

sum(b$t >= 4.5)

sum(b$t >= 4.5) / 1000

quantile(b$t, 0.95)

anova(fit.null, fit.x, test = "Chisq") %>>% print()

devtools::session_info()
