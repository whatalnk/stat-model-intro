
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

post.jags.df %>>% 
    {map2((.)$beta1, (.)$beta2, function(beta1, beta2){
        stat_function(mapping = aes(x), data = data_frame(x = c(3, 7)), geom = "line", fun = function(x, beta1, beta2){
            exp(beta1 + beta2 * x)
        }, args = list(beta1 = beta1, beta2 = beta2), alpha = 0.5)
    })} -> lambdas

options(repr.plot.width = 4, repr.plot.height = 4)

lambdas[[1]] 

lambdas[[2]] 

post.jags.df %>>% head(10)

post.jags.df %>>% ggplot(aes(x = beta1, y = beta2)) + geom_point(alpha = 0.5)

exp(2.133491 + 0.05784056* c(3:7))

exp(1.5 + 0.1 * c(3:7))

options(expressions = 50000)

ggplot() + 
    lambdas + 
#     scale_y_continuous(breaks = seq(4, 12, 2), limits = c(3, 12)) + 
    theme_bw() + theme(panel.grid = element_blank())

summary(post.jags)

coda::gelman.diag(post.jags)

options(repr.plot.width = 8)

coda::gelman.plot(post.jags)
