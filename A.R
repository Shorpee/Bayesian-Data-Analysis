library(rethinking)
d <- read.csv("https://raw.githubusercontent.com/torkar/dat321/master/data_autumn2020.csv", sep=";") # nolint

d$technique[d$technique == "0T"] <- "OT" #Fix the error

dat_list <- list(
  tp = d$tp,
  T = ifelse(d$technique == "NT", 1L, 2L),
  C = ifelse(d$category == "LE", 1L, 2L)
)

 

# m <- ulam(
#     alist (
#         tp ~ dpois(lamda),
#         log(lamda) <- a + bc[C] + bt[T],
#         a ~ dnorm(1.5,.2),
#         bc[C] ~ dnorm(0,.2),
#         bt[T] ~ dnorm(0,.2)
#     ),data=dat_list,chains=4,cores=4,iter = 3000,log_lik=TRUE
# )

# mg <- ulam(
#     alist (
#         tp ~ dgampois(lamda,phi),
#         log(lamda) <- a + bc[C] + bt[T],
#         a ~ dnorm(1.5,.2),
#         bc[C] ~ dnorm(0,.2),
#         bt[T] ~ dnorm(0,.2),
#         phi ~ dexp(1)
#     ),data=dat_list,chains=4,cores=4,cmdstan = TRUE,iter = 3000,log_lik=TRUE
# )

# compare(m,mg,func=WAIC) # choose less WAIC


# PriorPlot <-exp(prior$a + prior$bc[1] + prior$bt[1])



mT <- ulam(
    alist (
        tp ~ dpois(lamda),
        log(lamda) <- a + bt[T],
        a ~ dnorm(1.5,.2),
        bc[C] ~ dnorm(0,.2),
        bt[T] ~ dnorm(0,.2)
    ),data=dat_list,chains=4,cores=4,iter = 3000,log_lik=TRUE
)

mC <- ulam(
    alist (
        tp ~ dpois(lamda),
        log(lamda) <- a + bc[C],
        a ~ dnorm(1.5,.2),
        bc[C] ~ dnorm(0,.2),
        bt[T] ~ dnorm(0,.2)
    ),data=dat_list,chains=4,cores=4,iter = 3000,log_lik=TRUE
)


#8
compare(mT,mC, func=WAIC)  # model with Tech is better and we should use cat



posterior <-extract.samples(mT)
different <- posterior$bt[,2] - posterior$bt[,1]
precis(different,depth=2)