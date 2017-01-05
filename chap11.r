
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
    xlab(expression("Position "*italic("y"))) + 
    ylab(expression("Number of individuals  "*italic("y")[italic("j")])) + 
    ylim(c(0, 25))

mean(Y)

var(Y)

readLines("chap11-model.jags", encoding = "UTF-8") %>>% cat(sep = "\n")

library(rjags)

data.list <- list(
    N.site = length(Y), 
    Y = Y, 
    Adj = c(2, 2:(length(Y)-1), length(Y)-1) %>>% 
            purrr::map_at(c(2:49), function(x){c(x - 1, x + 1)}) %>>% 
            purrr::flatten() %>>% purrr::flatten_dbl(), 
    Weights = rep(1, 2 * length(Y) - 2), 
    Num = c(1, rep(2, length(Y) - 2), 1)
)

data.list

inits.list <- list(
    beta = 0, 
    r = rnorm(length(Y), 0, 0.1), 
    s = 1
)

inits.list

c(sapply(2:(50 - 1), function(a) c(a - 1, a + 1))) %>>% 
    {c(NA, 2, (.), 49, NA)} %>>%
    {matrix(data = (.), ncol = 2, byrow = TRUE)} %>>% 
    data.frame() %>>% 
    mutate(j = c(1:50)) %>>% 
    rename(r1 = X1, r2 = X2) %>>% 
    select(j, r1, r2) %>>% head()

m <- jags.model(
    file = "chap11-model.jags", 
    data = data.list, 
    inits = list(inits.list, inits.list, inits.list), 
    n.chain = 3
)

library(CARBayes)

load("Y.RData")

ls.str()

options(repr.plot.width = 4, repr.plot.height = 4)

plot(Y)

n <- length(Y)
W <- matrix(0, nrow = n, ncol = n)

for (i in 2:n) {
    W[i, i - 1] <- W[i - 1, i] <- 1
}

fit <- S.CARleroux(Y ~ 1, family = "poisson", W = W, burnin = 2000, n.sample = 32000, thin = 10, 
                   fix.rho = TRUE, rho = 1)

print(fit)

summarise.samples(fit$samples$beta, quantiles = c(0.025, 0.5, 0.975))

summarise.samples(fit$samples$phi, quantiles = c(0.025, 0.5, 0.975))

library(coda)

options(repr.plot.width = 8, repr.plot.height = 4)

geweke.diag(fit$samples$beta)
geweke.diag(fit$samples$phi)

plot(fit$samples$beta, las = 1)
plot(fit$samples$phi[,49], las = 1)


sapply(c("ggplot2", "Cairo"), require, character.only = TRUE)

options(repr.plot.width = 4, repr.plot.height = 4)

seq_along(c(1,10, 20))

point.alpha <- 0.5
ribbon.alpha <- 0.3
ribbon.fill <- "black"
Y.fit <- apply(fit$samples$fitted, 2, mean) #3000回の平均
Y.ci <- apply(fit$samples$fitted, 2, quantile, c(0.025, 0.975))

df <- data.frame(j = seq_along(Y), Y = Y, Yhat = Y.fit,
                  Yupper = Y.ci["97.5%", ], Ylower = Y.ci["2.5%", ])
p <- ggplot(df) +
  geom_ribbon(aes(x = j, ymax = Yupper, ymin = Ylower),
              fill = ribbon.fill, alpha = ribbon.alpha) +
  geom_point(aes(x = j, y = Y), size = 2.5) +
  geom_line(aes(x = j, y = Yhat), size = 0.5) +
  xlab("位置") + ylab("個体数") +
  ylim(0, 25) +
  theme_classic(base_family = "IPAexGothic")

Cairo(type = "raster")
print(p)
dev.off()

str(fit$samples$fitted)

head(fit$samples$fitted, 5)

print(readLines("runbugs.R"))
