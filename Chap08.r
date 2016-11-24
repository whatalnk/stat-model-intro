
date()

sapply(c("pipeR", "dplyr", "tidyr", "ggplot2", "readr"), require, character.only = TRUE)

load("data/chap08/data.RData")
# or
# data <- c(4,3,4,5,5,2,3,1,4,0,1,5,5,6,5,4,4,5,3,4)
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

library("R6")

MCMC <- R6Class("MCMC",
    public = list(
        data = NULL, 
        vq = seq(0.01, 0.99, 0.01), 
        initialize = function(data = NA) {
            self$data <- data
        }
    ), 
    private = list(
        logL = function(q){
            sum(self$data * log(q) + (8 - self$data) * log(1 - q) + log(choose(8, self$data)))
        } 
    )
  
)

MCMC.walk <- R6Class("MCMC.walk",
    inherit = MCMC, 
    public = list(
        doMCMC = function(start, nstep){
            qs <- numeric(nstep)
            qs[1] <- start
            pL <- super$logL(start)
            for(i in c(2:nstep)){
                q_L <- private$generate_step(qs[i - 1], pL)
                qs[i] <- q_L$q
                pL <- q_L$L
            }
            return(qs)
        }
    ), 
    private = list(
        generate_step = function(q, L){
            nq <- dplyr::if_else(sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5)), q - 0.01, q + 0.01)
            nL <- super$logL(nq)
            if (nL > L){
                return(list(q = nq, L = nL))
            } else {
                return(list(q = q, L = L))
            }
        }
    )
  
)

mcmc.walk <- MCMC.walk$new(data)
mcmc.walk

mcmc.walk$data

mcmc.walk$vq %>>% head()

mcmc.walk$doMCMC(0.3, 100) %>>% last()

data_frame(nstep = c(1:100), q03 = mcmc.walk$doMCMC(0.3, 100), q06 = mcmc.walk$doMCMC(0.6, 100)) %>>% 
    gather(start, q, -nstep) %>>% 
    ggplot(aes(x = nstep, y = q, group = start, linetype = start)) + 
        geom_line() + 
        ylim(c(0.25, 0.65)) + 
        theme(
            legend.position = c(.9, .85)
        ) + 
        scale_linetype(labels = c("0.30", "0.60"))

MCMC.metropolis <- R6Class("MCMC.metropolis",
    inherit = MCMC, 
    public = list(
        doMCMC = function(start, nstep){
            qs <- numeric(nstep)
            qs[1] <- start
            pL <- super$logL(start)
            for(i in c(2:nstep)){
                q_L <- private$generate_step(qs[i - 1], pL)
                qs[i] <- q_L$q
                pL <- q_L$L
            }
            return(qs)
        } 
    ), 
    private = list(
        generate_step = function(q, L){
            nq <- dplyr::if_else(sample(c(TRUE, FALSE), size = 1, prob = c(0.5, 0.5)), q - 0.01, q + 0.01)
            nL <- super$logL(nq)
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
    ) 
)

mcmc.metropolis <- MCMC.metropolis$new(data)
mcmc.metropolis

mcmc.metropolis$doMCMC(0.3, 10)

library(gridExtra)

options(repr.plot.width = 10, repr.plot.height = 3)

d.met1 <- data_frame(
    nstep = c(1:100), 
    q = mcmc.metropolis$doMCMC(0.3, 100)
)
gp1 <- ggplot(data = d.met1, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met1, aes(x = q)) + geom_histogram(binwidth = 0.01) + xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

d.met2 <- data_frame(
    nstep = c(1:1000), 
    q = mcmc.metropolis$doMCMC(0.3, 1000)
)
gp1 <- ggplot(data = d.met2, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met2, aes(x = q)) + geom_histogram(binwidth = 0.01) + xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

d.met3 <- data_frame(
    nstep = c(1:100000), 
    q = mcmc.metropolis$doMCMC(0.3, 100000)
)
gp1 <- ggplot(data = slice(d.met3, seq(1, 100000, 100)), aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = d.met3, aes(x = q)) + geom_histogram(binwidth = 0.01) + xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

d.met4 <- data_frame(
    nstep = c(1:500), 
    q1 = mcmc.metropolis$doMCMC(0.1, 500), 
    q3 = mcmc.metropolis$doMCMC(0.3, 500), 
    q4 = mcmc.metropolis$doMCMC(0.4, 500), 
    q45 = mcmc.metropolis$doMCMC(0.45, 500),    
    q5 = mcmc.metropolis$doMCMC(0.5, 500), 
    q6 = mcmc.metropolis$doMCMC(0.6, 500), 
    q7 = mcmc.metropolis$doMCMC(0.7, 500) 
)

options(repr.plot.width = 8, repr.plot.height = 4)

d.met4 %>>% gather(start, q, -nstep) %>>% 
    ggplot(aes(x = nstep, y = q, group = start, colour = start)) + 
    geom_line() + 
    scale_colour_discrete(labels = c("0.1", "0.3", "0.4", "0.45", "0.5", "0.6", "0.7")) 

MCMC.metropolis$set("public", "st.dist", function(){
    vlogL <- sapply(self$vq, function(x){super$logL(x)})
    data_frame(vq = self$vq, st.dist = exp(vlogL) / sum(exp(vlogL)))
}, overwrite = TRUE)
MCMC.metropolis

mcmc.metropolis.st <- MCMC.metropolis$new(data)
dd <- mcmc.metropolis.st$st.dist()

met.freq.q <- function(vq, q){
    lcount <- hist(q, breaks = seq(min(vq) - 0.01 * 0.5, max(vq) + 0.01 * 0.5, 0.01), plot = FALSE)$count
    data_frame(vq, dens = lcount / sum(lcount))
}

options(repr.plot.width = 10, repr.plot.height = 3)

gp1 <- ggplot(data = d.met1, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = met.freq.q(mcmc.metropolis$vq, d.met1$q), aes(x = vq, y = dens)) + 
    geom_bar(stat = "identity") + 
    geom_line(data = dd, mapping = aes(x = vq, y = st.dist)) + 
    xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

gp1 <- ggplot(data = d.met2, aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = met.freq.q(mcmc.metropolis$vq, d.met2$q), aes(x = vq, y = dens)) + 
    geom_bar(stat = "identity") + 
    geom_line(data = dd, mapping = aes(x = vq, y = st.dist)) + 
    xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

gp1 <- ggplot(data = slice(d.met3, seq(1, 100000, 100)), aes(x = nstep, y = q)) + geom_line() + ylim(c(0.25, 0.65))
gp2 <- ggplot(data = met.freq.q(mcmc.metropolis$vq, d.met3$q), aes(x = vq, y = dens)) + 
    geom_bar(stat = "identity") + 
    geom_line(data = dd, mapping = aes(x = vq, y = st.dist)) + 
    xlim(c(0.25, 0.65))
grid.arrange(gp1, gp2, ncol = 2)

devtools::session_info()
