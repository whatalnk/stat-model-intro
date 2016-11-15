
date()

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

load("data/chap08/data.RData")
str(data)

options(repr.plot.width = 4, repr.plot.height = 4)

table(data)

sum(data)

ggplot(data_frame(x = c(0, 8)), aes(x)) + 
    geom_histogram(data = data_frame(y = data), mapping = aes(x = y), binwidth = 1, colour = "white") + 
    stat_function(geom="point", n=9, fun = function(x, size, prob){20 * dbinom(x, size, prob)}, 
                  args = list(size = 8, prob = 0.45), shape = 21, fill = "white") + 
    xlab(expression("y"[i]))

choose(8, 2)

logL <- function(q, y){
    sum(y * log(q) + (8 - y) * log(1 - q) + log(choose(8, y)))
}

logL(0.46, data)

data_frame(q = seq(0.2, 0.7, 0.01)) %>>% 
    mutate(logL = sapply(q, function(x){logL(x, data)})) %>>% 
    ggplot(aes(x = q, y = logL)) + 
    geom_line() + 
    geom_vline(xintercept = 0.46, linetype = "dotted")

73 / 160

step_logL <- function(q, L){
    nq <- ifelse(sample(c(0, 1), size = 1, prob = c(0.5, 0.5)) == 0, q - 0.01, q + 0.01)
    nL <- logL(nq, data)
    if (nL > L){
        return(list(q = nq, L = nL))
    } else {
        return(list(q = q, L = L))
    }
}

do_step_logL <- function(start, nstep){
    qs <- numeric(nstep)
    qs[1] <- start
    pL <- logL(start, data)
    for(i in c(2:nstep)){
        q_L <- step_logL(qs[i - 1], pL)
        qs[i] <- q_L$q
        pL <- q_L$L
    }
    return(qs)
}

data_frame(nstep = c(1:100), q03 = do_step_logL(0.3, 100), q06 = do_step_logL(0.6, 100)) %>>% 
    gather(start, q, -nstep) %>>% 
    ggplot(aes(x = nstep, y = q, group = start, linetype = start)) + 
        geom_line() + 
        ylim(c(0.25, 0.65)) + 
        theme(
            legend.position = c(.9, .85)
        ) + 
        scale_linetype(labels = c("0.30", "0.60"))

step_metropolis <- function(q, L){
    nq <- ifelse(sample(c(0, 1), size = 1, prob = c(0.5, 0.5)) == 0, q - 0.01, q + 0.01)
    nL <- logL(nq, data)
    if (nL > L){
        return(list(q = nq, L = nL))
    } else {
        r <- exp(nL - L)
        if(sample(c(0, 1), size = 1, prob = c(r, 1 - r)) == 0){
            return(list(q = nq, L = nL))
            
        } else {
            return(list(q = q, L = L))            
        }
    }
}

metropolis <- function(start, nstep){
    qs <- numeric(nstep)
    qs[1] <- start
    pL <- logL(start, data)
    for(i in c(2:nstep)){
        q_L <- step_metropolis(qs[i - 1], pL)
        qs[i] <- q_L$q
        pL <- q_L$L
    }
    return(qs)
}

library(gridExtra)

options(repr.plot.width = 10, repr.plot.height = 2)

# d.met1 <- data_frame(
#     nstep = c(1:100), 
#     q = metropolis(0.3, 100)
# )
gp1 <- ggplot(data = d.met1, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met1, aes(q)) + geom_density() + xlim(c(0, 1))
grid.arrange(gp1, gp2, ncol = 2)

# d.met2 <- data_frame(
#     nstep = c(1:1000), 
#     q = metropolis(0.3, 1000)
# )
gp1 <- ggplot(data = d.met2, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met2, aes(q)) + geom_density() + xlim(c(0,1))
grid.arrange(gp1, gp2, ncol = 2)

# d.met3 <- data_frame(
#     nstep = c(1:100000), 
#     q = metropolis(0.3, 100000)
# )
gp1 <- ggplot(data = slice(d.met3, seq(1, 100000, 100)), aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = slice(d.met3, seq(1, 100000, 100)), aes(q)) + geom_density() + xlim(c(0, 1))
grid.arrange(gp1, gp2, ncol = 2)
