
date()

sapply(c("pipeR", "dplyr", "tidyr", "purrr", "ggplot2", "readr"), require, character.only= TRUE)

load("data/chap11/Y.RData")
ls.str()

options(repr.plot.width = 4,repr.plot.height = 3)

# Fig. 11.2
data_frame(j = seq_along(Y), y = Y) %>>% 
    ggplot(aes(x = j, y = y)) + 
    geom_point(shape = 21)+ 
    theme_bw() +
    theme(panel.grid = element_blank()) + 
    xlab(expression("Position "*italic("j"))) + 
    ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
    ylim(c(0, 25))

mean(Y)

var(Y)

readLines("chap11-model.jags", encoding = "UTF-8") %>>% cat(sep = "\n")

readLines("chap11-model2.jags", encoding = "UTF-8") %>>% cat(sep = "\n")

library(rjags)

data.list2 <- list(
    N.site = length(Y), 
    Y = Y
)

data.list2

m2 <- jags.model(
    file = "chap11-model2.jags", 
    data = data.list,
    n.chain = 3, 
    n.adapt = 100
)

post.jags2 <- coda.samples(m, variable.names = c("r", "s"), n.iter = 10000, thin = 10)

saveRDS(post.jags2, file = "chap11-post-jags2.rds")

post.jags2 <- readRDS(file = "chap11-post-jags2.rds")

summary(post.jags2)

gelman.diag(post.jags2)

varnames(post.jags2)

Y.hat <- exp(summary(post.jags2[, grep("r", varnames(post.jags2))])$statistics[, "Mean"])
Y.qnt <- exp(summary(post.jags2[, grep("r", varnames(post.jags2))])$quantiles[, c("2.5%", "97.5%")])

data_frame(site = seq_along(Y), 
           Y = Y,
           Y.hat = Y.hat,
           Y.975 = Y.qnt[, 2],
           Y.025 = Y.qnt[, 1]) %>>% 
    ggplot() +
        geom_point(aes(x = site, y = Y)) +
        geom_line(aes(x = site, y = Y.hat)) +
        geom_ribbon(aes(x = site, ymin = Y.025, ymax = Y.975), alpha = 0.3) + 
        theme_bw() + 
        theme(
            panel.grid = element_blank()
        ) + 
        xlab(expression("Position "*italic("j"))) + 
        ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
        ylim(c(0, 25))

data.list3 <- list(
    N.site = length(Y), 
    Y = matrix(c(Y, Y, Y), length(Y), 3), 
    Tau = c(1000, 20, 0.01)
)

m3 <- jags.model(
    file = "chap11-model3.jags", 
    data = data.list3,
    n.chain = 3, 
    n.adapt = 100
)

post.jags3 <- coda.samples(m3, variable.names = c("r"), n.iter = 200, thin = 1)

saveRDS(post.jags3, file = "chap11-post-jags3.rds")

post.jags3 <- readRDS("chap11-post-jags3.rds")

summary(post.jags3)

str(post.jags3)

post.jags3 %>>% purrr::map_df(function(x){as_data_frame(x)}) %>>% dplyr::sample_n(3) %>>% 
    select(ends_with("1]")) %>>% t() %>>% as_data_frame() %>>% 
    purrr::dmap(function(x){exp(x)}) %>>% 
    mutate(site = seq_along(Y), Y = Y) %>>% 
    ggplot() + 
        geom_point(aes(x = site, y = Y)) + 
        geom_line(aes(x = site, y = V1), colour = gray(0.6)) + 
        geom_line(aes(x = site, y = V2), colour = gray(0.65)) + 
        geom_line(aes(x = site, y = V3), colour = gray(0.7)) + 
        theme_bw() + theme(panel.grid = element_blank()) + 
        xlab(expression("Position "*italic("j"))) + 
        ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
        ylim(c(0, 25))

post.jags3 %>>% purrr::map_df(function(x){as_data_frame(x)}) %>>% dplyr::sample_n(3) %>>% 
    select(ends_with("2]")) %>>% t() %>>% as_data_frame() %>>% 
    purrr::dmap(function(x){exp(x)}) %>>% 
    mutate(site = seq_along(Y), Y = Y) %>>% 
    ggplot() + 
        geom_point(aes(x = site, y = Y)) + 
        geom_line(aes(x = site, y = V1), colour = gray(0.6)) + 
        geom_line(aes(x = site, y = V2), colour = gray(0.65)) + 
        geom_line(aes(x = site, y = V3), colour = gray(0.7)) + 
        theme_bw() + theme(panel.grid = element_blank()) + 
        xlab(expression("Position "*italic("j"))) + 
        ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
        ylim(c(0, 25))

post.jags3 %>>% purrr::map_df(function(x){as_data_frame(x)}) %>>% dplyr::sample_n(3) %>>% 
    select(ends_with("3]")) %>>% t() %>>% as_data_frame() %>>% 
    purrr::dmap(function(x){exp(x)}) %>>% 
    mutate(site = seq_along(Y), Y = Y) %>>% 
    ggplot() + 
        geom_point(aes(x = site, y = Y)) + 
        geom_line(aes(x = site, y = V1), colour = gray(0.6)) + 
        geom_line(aes(x = site, y = V2), colour = gray(0.65)) + 
        geom_line(aes(x = site, y = V3), colour = gray(0.7)) + 
        theme_bw() + theme(panel.grid = element_blank()) + 
        xlab(expression("Position "*italic("j"))) + 
        ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
        ylim(c(0, 25))

Idx.obs <- c(1:50)[-c(6, 9, 12, 13, 26:30)]
Y.obs <- Y[Idx.obs]
data.list4 <- list(
    N.site = length(Y), 
    Idx.obs = Idx.obs, 
    Y.obs = Y.obs, 
    N.obs = length(Y.obs)
)

m4 <- jags.model(
    file = "chap11-model4.jags", 
    data = data.list4,
    n.chain = 3, 
    n.adapt = 100
)

post.jags4 <- coda.samples(
    m2, 
    c("r", "s"), 
    n.iter = 10000, 
    thin = 10
)

(function(){
    Y.hat <- exp(summary(post.jags4[, grep("r", varnames(post.jags4))])$statistics[, "Mean"])
    Y.qnt <- exp(summary(post.jags4[, grep("r", varnames(post.jags4))])$quantiles[, c("2.5%", "97.5%")])
    p.fill <- rep("black", 50)
    p.fill[Idx.obs] <- "white"
    data_frame(site = seq_along(Y), 
           Y = Y,
           Y.hat = Y.hat,
           Y.975 = Y.qnt[, 2],
           Y.025 = Y.qnt[, 1]) %>>% 
    ggplot() +
        geom_point(aes(x = site, y = Y), shape = 21, fill = p.fill) +
        geom_line(aes(x = site, y = Y.hat)) +
        geom_ribbon(aes(x = site, ymin = Y.025, ymax = Y.975), alpha = 0.3) + 
        theme_bw() + 
        theme(
            panel.grid = element_blank()
        ) + 
        xlab(expression("Position "*italic("j"))) + 
        ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
        scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 27))
})()

m5 <- jags.model(
    file = "chap11-model5.jags", 
    data = data.list4,
    n.chain = 3, 
    n.adapt = 100
)

post.jags5 <- coda.samples(
    m5, 
    c("r", "beta", "s"), 
    n.iter = 10000,
    thin = 10
)

gelman.diag(post.jags5)

summary(post.jags5)

(function(){
    beta <- summary(post.jags5[, "beta"])$statistics[["Mean"]]
    Y.hat <- exp(beta + summary(post.jags5[, grep("r", varnames(post.jags5))])$statistics[, "Mean"])
    Y.qnt <- exp(beta + summary(post.jags5[, grep("r", varnames(post.jags5))])$quantiles[, c("2.5%", "97.5%")])
    p.fill <- rep("black", 50)
    p.fill[Idx.obs] <- "white"
    data_frame(site = seq_along(Y), 
           Y = Y,
           Y.hat = Y.hat,
           Y.975 = Y.qnt[, 2],
           Y.025 = Y.qnt[, 1]) %>>% 
    ggplot() +
        geom_point(aes(x = site, y = Y), shape = 21, fill = p.fill) +
        geom_line(aes(x = site, y = Y.hat)) +
        geom_ribbon(aes(x = site, ymin = Y.025, ymax = Y.975), alpha = 0.3) + 
        theme_bw() + 
        theme(
            panel.grid = element_blank()
        ) + 
        xlab(expression("Position "*italic("j"))) + 
        ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
        scale_y_continuous(breaks = seq(0, 25, 5), limits = c(0, 28))
})()

devtools::session_info()
