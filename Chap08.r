
date()

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

load("data/chap08/data.RData")
str(data)

table(data)

sum(data)

options(repr.plot.width = 4, repr.plot.height = 4)

ggplot(data_frame(x = c(0, 8)), aes(x)) + 
    geom_histogram(data = data_frame(y = data), mapping = aes(x = y), binwidth = 1, colour = "white") + 
    xlab(expression("y"[i]))

logL <- function(q, y){
    sum(y * log(q) + (8 - y) * log(1 - q) + log(choose(8, y)))
}

logL(0.46, data)

data_frame(q = seq(0.2, 0.7, 0.01)) %>>% 
    mutate(logL = sapply(q, function(x){logL(x, data)})) %>>% 
    ggplot(aes(x = q, y = logL)) + 
    geom_line() + 
    geom_vline(xintercept = 0.46, linetype = "dotted")

sum(data) / (8 * 20)

ggplot(data_frame(x = c(0, 8)), aes(x)) + 
    geom_histogram(data = data_frame(y = data), mapping = aes(x = y), binwidth = 1, colour = "white") + 
    stat_function(geom="point", n=9, fun = function(x, size, prob){20 * dbinom(x, size, prob)}, 
                  args = list(size = 8, prob = 0.45), shape = 21, fill = "white", size = 2) + 
    xlab(expression("y"[i]))

step_logL <- function(q, L, data){
    nq <- ifelse(sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5)), q - 0.01, q + 0.01)
    nL <- logL(nq, data)
    if (nL > L){
        return(list(q = nq, L = nL))
    } else {
        return(list(q = q, L = L))
    }
}

do_step_logL <- function(start, nstep, data){
    qs <- numeric(nstep)
    qs[1] <- start
    pL <- logL(start, data)
    for(i in c(2:nstep)){
        q_L <- step_logL(qs[i - 1], pL, data)
        qs[i] <- q_L$q
        pL <- q_L$L
    }
    return(qs)
}

do_step_logL(0.3, 100, data) %>>% last()

data_frame(nstep = c(1:100), q03 = do_step_logL(0.3, 100, data), q06 = do_step_logL(0.6, 100, data)) %>>% 
    gather(start, q, -nstep) %>>% 
    ggplot(aes(x = nstep, y = q, group = start, linetype = start)) + 
        geom_line() + 
        ylim(c(0.25, 0.65)) + 
        theme(
            legend.position = c(.9, .85)
        ) + 
        scale_linetype(labels = c("0.30", "0.60"))

step_metropolis <- function(q, L, data){
    nq <- ifelse(sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5)), q - 0.01, q + 0.01)
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

metropolis <- function(start, nstep, data){
    qs <- numeric(nstep)
    qs[1] <- start
    pL <- logL(start, data)
    for(i in c(2:nstep)){
        q_L <- step_metropolis(qs[i - 1], pL, data)
        qs[i] <- q_L$q
        pL <- q_L$L
    }
    return(qs)
}

step_metropolis(0.3, L = logL(0.4, data), data)

metropolis(start = 0.3, nstep = 10, data = data)

library(gridExtra)

options(repr.plot.width = 10, repr.plot.height = 3)

d.met1 <- data_frame(
    nstep = c(1:100), 
    q = metropolis(0.3, 100, data)
)
gp1 <- ggplot(data = d.met1, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met1, aes(x = q)) + geom_histogram(binwidth = 0.01) + xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

d.met2 <- data_frame(
    nstep = c(1:1000), 
    q = metropolis(0.3, 1000, data)
)
gp1 <- ggplot(data = d.met2, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met2, aes(x = q)) + geom_histogram(binwidth = 0.01) + xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

d.met3 <- data_frame(
    nstep = c(1:100000), 
    q = metropolis(0.3, 100000, data)
)
gp1 <- ggplot(data = slice(d.met3, seq(1, 100000, 100)), aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met3, aes(x = q)) + geom_histogram(binwidth = 0.01) + xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

d.met4 <- data_frame(
    nstep = c(1:500), 
    q1 = metropolis(0.1, 500, data), 
    q3 = metropolis(0.3, 500, data), 
    q4 = metropolis(0.4, 500, data), 
    q45 = metropolis(0.45, 500, data),    
    q5 = metropolis(0.5, 500, data), 
    q6 = metropolis(0.6, 500, data), 
    q7 = metropolis(0.7, 500, data) 
)

options(repr.plot.width = 8, repr.plot.height = 4)

d.met4 %>>% gather(start, q, -nstep) %>>% 
    ggplot(aes(x = nstep, y = q, group = start, colour = start)) + 
    geom_line() + 
    scale_colour_discrete(labels = c("0.1", "0.3", "0.4", "0.45", "0.5", "0.6", "0.7")) 

st.dist <- function(data, size, vq){
    vlogL <- sapply(vq, function(x){logL(x, data)})
    exp(vlogL) / sum(exp(vlogL))
}

vq <- seq(0.01, 0.99, 0.01)
ddL <- data_frame(vq, st.dist = st.dist(data, 8, vq))

met.freq.q <- function(q){
    lcount <- hist(q, breaks = seq(min(vq) - 0.01 * 0.5, max(vq) + 0.01 * 0.5, 0.01), plot = FALSE)$count
    lcount / sum(lcount)
}

options(repr.plot.width = 10, repr.plot.height = 3)

gp1 <- ggplot(data = d.met1, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = data_frame(vq, dens = met.freq.q(d.met1$q)), aes(x = vq, y = dens)) + 
    geom_bar(stat = "identity") + 
    geom_line(data = ddL, mapping = aes(x = vq, y = st.dist)) + 
    xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

gp1 <- ggplot(data = d.met2, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = data_frame(vq, dens = met.freq.q(d.met2$q)), aes(x = vq, y = dens)) + 
    geom_bar(stat = "identity") + 
    geom_line(data = ddL, mapping = aes(x = vq, y = st.dist)) + 
    xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

gp1 <- ggplot(data = slice(d.met3, seq(1, 100000, 100)), aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = data_frame(vq, dens = met.freq.q(d.met3$q)), aes(x = vq, y = dens)) + 
    geom_bar(stat = "identity") + 
    geom_line(data = ddL, mapping = aes(x = vq, y = st.dist)) + 
    xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

devtools::session_info()
