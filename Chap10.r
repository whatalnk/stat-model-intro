
date()

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

d <- read_csv("data/chap10/data7a.csv")

str(d)
summary(d)

options(repr.plot.width=4, repr.plot.height=4)

#fig. 10.1(B)
d %>>%
    {ggplot(data = (.), aes(x = y)) + 
    stat_count(geom = "point", shape = 21) + 
    stat_function(geom="point", n=9, fun = function(x, size, prob){dbinom(x, size = size, prob = prob) * nrow(.)}, 
                  args = list(size = 8, prob = 0.504)) + 
    stat_function(geom="line", n=9, fun = function(x, size, prob){dbinom(x, size = size, prob = prob) * nrow(.)}, 
                  args = list(size = 8, prob = 0.504)) + 
    xlab("Number of living seeds") + ylab("Number of individuals") + ylim(c(0, 28))}

d %>>% summarize_each(funs(mean, var), y)

logL <- function(q, y){
    sum(y * log(q) + (8 - y) * log(1 - q) + log(choose(8, y)))
}

data_frame(q = seq(0.2, 0.7, 0.01)) %>>% 
    mutate(logL = sapply(q, function(x){logL(x, d$y)})) %>>% 
    ggplot(aes(x = q, y = logL)) + 
    geom_line()

sum(d$y) / (8 * 100)

8 * 0.504 * (1 - 0.504)

readLines("chap10-model.jags") %>>% cat(sep = "\n")

data.list <- list(
    N = nrow(d), 
    Y = d$y
)

inits.list <- list(
    beta = 0, 
    r = rnorm(nrow(d), 0, 0.1), 
    s = 1
)

library(rjags)

m <- jags.model(
    file = "chap10-model.jags",
    data = data.list,
    inits = list(inits.list, inits.list, inits.list),
    n.chain = 3
)

update(m, 100)

post.jags <- coda.samples(
    m, 
    c("beta", "r", "s"), 
    thin = 10, 
    n.iter = 10000
)

saveRDS(post.jags, file = "chap10-post-jags.rds")

post.jags <- readRDS(file = "chap10-post-jags.rds")

summary(post.jags)

str(post.jags)

class(post.jags)

varnames(post.jags)

varnames(post.jags)

options(repr.plot.width = 8, repr.plot.height = 6)

post.jags[,c("beta", "s"), drop=FALSE] %>>% plot()

options(repr.plot.width = 8, repr.plot.height = 9)

post.jags[,c("r[1]","r[2]","r[3]"), drop=FALSE] %>>% plot()

coda::gelman.diag(post.jags)

varnames(post.jags)

paste0("r[", c(1:100), "]")

summary(post.jags)$quantiles[paste0("r[", c(1:100), "]"), "50%"]

summary(post.jags)$quantiles["beta", "50%"] %>>% class()

logistic <- function(beta, r){1 / (1 + exp(-(beta + r)))}
# r についての積分

integrand <- function(r, x, size, beta, s){
    dbinom(x, size = size, prob = logistic(beta, r)) * dnorm(r, mean = 0, sd = s)
}
# 各種子数について -Inf--Infまで積分
living_seed <- function(x, size, beta, s){
    sapply(x, function(xx){
        integrate(f = integrand, lower = -Inf, upper = Inf, 
                  x = xx, size = size, beta = beta, s = s
        )$value
    })
}
living_seed(x = 0:8, size = 8, beta = summary(post.jags)$quantiles["beta", "50%"], s = summary(post.jags)$quantiles["s", "50%"])

library(purrr)

post.jags[, c("beta", "s")] %>>% 
    purrr::map_df(function(x){as_data_frame(x)}) %>>% 
    {purrr::map2((.)$beta, (.)$s, function(x, y){
        living_seed(x = 0:8, size = 8, beta = x, s = y)})
    } %>>% 
    purrr::map_df(function(x){
        xx <- sample(0:8, 100, replace = TRUE, prob = x) %>>% factor(levels = 0:8) %>>% summary()
        d <- data.frame(matrix(xx, nrow = 1))
        names(d) <- c(0:8)
        d
    }) %>>% 
    purrr::dmap(quantile, probs = c(0.025, 0.975, 0.25, 0.75)) -> post.jags.quantiles
post.jags.quantiles

post.jags.quantiles[1, ] %>>% as.numeric() %>>% str()

#fig. 10.4
ggplot() + 
    geom_ribbon(aes(x = c(0:8), ymin = as.numeric(post.jags.quantiles[1, ]), 
                        ymax = as.numeric(post.jags.quantiles[2, ])), fill = gray(0.8)) + 
    geom_ribbon(aes(x = c(0:8), ymin = as.numeric(post.jags.quantiles[3, ]), 
                        ymax = as.numeric(post.jags.quantiles[4, ])), fill = gray(0.6)) + 
        stat_count(data = d, mapping = aes(x = y), geom = "point", shape = 19) + 
        stat_function(data = data_frame(x = c(0:8)), mapping = aes(x), geom="line", n=9, 
                      fun = function(x, size, beta, s){living_seed(x, size, beta, s) * 100}, 
                      args = list(size = 8, 
                                  beta = summary(post.jags)$quantiles["beta", "50%"], 
                                  s = summary(post.jags)$quantiles["s", "50%"])) + 
        stat_function(data = data_frame(x = c(0:8)), mapping = aes(x), geom="point", n=9, 
                      fun = function(x, size, beta, s){living_seed(x, size, beta, s) * 100}, 
                      args = list(size = 8, 
                                  beta = summary(post.jags)$quantiles["beta", "50%"], 
                                  s = summary(post.jags)$quantiles["s", "50%"]), 
                      shape = 21, fill = "white") + 
    xlab("Number of living seeds") + ylab("Number of individuals") + 
#     ylim(c(0, 28)) + 
    theme_bw() + theme(panel.grid = element_blank())

d1 <- read_csv("data/chap10/d1.csv")
str(d1)

{d1 %>>% group_by(f) %>>% summarize(mean(y))}[[2]]

d1 %>>%
    ggplot(aes(x = id, y = y, label = pot, group = f, colour = f)) + geom_text() + 
        geom_segment(aes(x = 0, y = mean((.)$y[(.)$f == "C"]), 
                         xend = 50, yend = mean((.)$y[(.)$f == "C"])), 
                     colour = "#0072B2", linetype = "dotted") + 
        geom_segment(aes(x = 51, y = mean((.)$y[(.)$f == "T"]), 
                         xend = 100, yend = mean((.)$y[(.)$f == "T"])), 
                     colour = "#CC79A7", linetype = "dotted") + 
        theme_bw() +     
        theme(
            legend.title = element_blank(),
            legend.background = element_blank(), 
            legend.position = c(.2, .9), 
            panel.grid = element_blank()
        ) +
        scale_colour_manual(labels = c("Control", "Treatment"), values = c("#0072B2", "#CC79A7"))

d1 %>>% ggplot(aes(x = pot, y = y, fill = f)) + geom_boxplot() + 
    theme_bw() + 
    theme(
        legend.title = element_blank(),
        legend.background = element_blank(), 
        legend.position = c(.2, .9), 
        panel.grid = element_blank()
    ) +
    scale_fill_manual(labels = c("Control", "Treatment"), values = c("#0072B2", "#CC79A7"))

readLines("chap10-model2.jags") %>>% cat(sep = "\n")

as.numeric(d1$f == "T")

data.list2 <- list(
    N.sample = nrow(d1), 
    N.pot = length(unique(d1$pot)), 
    N.tau = 2, 
    Y = d1$y, 
    F = as.numeric(d1$f == "T"), 
    Pot = as.numeric(as.factor(d1$pot))
)

inits.list2 <- list(
    beta1 = 0, 
    beta2 = 0, 
    s = c(1, 1), 
    r = rnorm(data.list2[["N.sample"]], 0, 0.1), 
    rp = rnorm(data.list2[["N.pot"]], 0, 0.1)
)

m2 <- jags.model(
    file = "chap10-model2.jags", 
    data = data.list2, 
    inits = list(inits.list2, inits.list2, inits.list2), 
    n.chain = 3
)

update(m2, 1000)

post.jags2 <- coda.samples(
    m2,
    c("beta1", "beta2",  "s", "r", "rp"), 
    thin = 50, n.iter = 50000
)

saveRDS(post.jags2, file = "chap10-post-jags2.rds")

post.jags2 <- readRDS(file = "chap10-post-jags2.rds")

summary(post.jags2)

gelman.diag(post.jags2)

options(repr.plot.width = 8, repr.plot.height = 6)

post.jags2[,c("beta1", "beta2"), drop=FALSE] %>>% plot()

options(repr.plot.width = 4, repr.plot.height = 4)

post.jags2[,c("beta1", "beta2")] %>>% 
    purrr::map_df(function(x){as_data_frame(x)}) %>>% 
    ggplot(aes(x = beta1, y = beta2)) + geom_point(alpha = 0.2) + theme_bw() + theme(panel.grid = element_blank())

devtools::session_info()
