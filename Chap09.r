
load("data/chap09/d.RData")

ls()

summary(d)

sapply(c("pipeR", "dplyr", "tidyr", "purrr", "ggplot2", "rjags"), require, character.only = TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)

ggplot(aes(x = x, y = y), data = d) + geom_point() + 
    stat_function(geom = "line", fun = function(x){exp(1.5 + 0.1 * x)}, linetype = "dotted") + 
    scale_y_continuous(breaks = seq(4, 12, 2), limits = c(3, 12))

readLines("chap09-model.jags") %>>% cat(sep = "\n")

data.list <- list(
    N = nrow(d), 
    Y = d$y, 
    X = d$x, 
    Mean.X = mean(d$x)
)

inits.list <- list(
    beta1 = 0, 
    beta2 = 0
)

m <- jags.model(
    file = "chap09-model.jags",
    data = data.list,
    inits = list(inits.list, inits.list, inits.list),
    n.chain = 3
)

update(m, 100)

post.jags <- coda.samples(
    m,
    c("beta1", "beta2"),
    thin = 3, n.iter = 1500
)

saveRDS(post.jags, file = "chap09-post-jags.rds")

str(post.jags)

summary(post.jags)

options(repr.plot.height = 6, repr.plot.width = 8)

plot(post.jags)

post.jags[[1]] %>>% str()

post.jags[[1]] %>>% attributes()

post.jags[[1]][, c("beta1", "beta2")] %>>% as_data_frame() %>>% str()

post.jags %>>% purrr::map_df(function(x){
    x[, c("beta1", "beta2")] %>>% as_data_frame()
}) %>>% str()

post.jags %>>% purrr::map_df(function(x){
    x[, c("beta1", "beta2")] %>>% as_data_frame()
}) -> post.jags.df

post.jags.df$beta1[[1]]

options(repr.plot.width = 4, repr.plot.height = 4)

post.jags.df %>>% head(10)

summary(post.jags)

summary(post.jags) %>>% str()

summary(post.jags)$quantiles["beta1", "50%"]

summary(post.jags)$quantiles["beta2", "50%"]

options(expressions = 50000)

post.jags.df %>>% 
    {map2((.)$beta1, (.)$beta2, function(beta1, beta2){
        stat_function(mapping = aes(x), 
                      data = data_frame(x = c(3, 7)), 
                      geom = "line", 
                      fun = function(x, beta1, beta2){
                          exp(beta1 + beta2 * (x - mean(d$x)))}, 
                      args = list(beta1 = beta1, beta2 = beta2), 
                      colour = gray(0.8, 0.1))
    })} -> lambdas

ggplot() + 
    lambdas +
    theme_bw() + theme(panel.grid = element_blank()) + 
    geom_point(data = d, mapping = aes(x = x, y = y)) + 
    stat_function(data = data_frame(x = c(3, 7)), 
                         mapping = aes(x), 
                         geom = "line", 
                         fun = function(x){
                             exp(summary(post.jags)$quantiles["beta1", "50%"] + 
                                 summary(post.jags)$quantiles["beta2", "50%"] * (x - mean(d$x)))}, 
                         colour = "black") + 
    scale_y_continuous(breaks = seq(4, 12, 2), limits = c(3, 12))

post.jags.df %>>% 
    ggplot(aes(x = beta1, y = beta2)) + 
        geom_point(colour = gray(0.5)) + 
        theme_bw() + theme(panel.grid = element_blank())

coda::gelman.diag(post.jags)

options(repr.plot.width = 8)

coda::gelman.plot(post.jags)
