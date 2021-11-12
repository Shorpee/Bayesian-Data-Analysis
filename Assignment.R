library(janitor)
library(dplyr)
library(tidyverse)
library(stringr)
library(rethinking)
library(dagitty)

#importing flat file as and reading it as a table
data <- read.csv("https://raw.githubusercontent.com/torkar/dat321/master/data_autumn2020.csv", sep=";", stringsAsFactors = FALSE)
min(data$tp)
max(data$tp)


#assign data to another variable to be cleaned
clean <- clean_names(data)
colnames(clean)


#use tabyl function to check each column and content
tabyl(clean, technique)

#use str_replace function to replace 0T with OT
#tabyl(clean, technique)
clean$technique = str_replace(clean$technique, "0T", "OT")


#assign the cleaned data to another variable
glimpse(clean)

#basic statistical analysis on data
min(clean$tp)
max(clean$tp)
mean(clean$tp)
median(clean$tp)
var(clean$tp)
sd(clean$tp)

hist(clean$tp)
dens(clean)
quantile(clean, 0.8)
precis(clean)

#data-list for cleaned data 
dat <- list(
  tp = clean$tp,
  Tech = ifelse(clean$technique== "OT", 1L, 2L),
  Cat = ifelse(clean$category== "LE", 1L, 2L)
)
glimpse(dat)

log(mean(clean$tp))
#intercept only Poisson model
m0_p <- ulam(
  alist(
    tp ~ dpois(lambda),
    log(lambda) <- a,
    a ~ dnorm(1.5, .2)
  ),data = dat, chains = 4, cores = 4, log_lik = TRUE
)
precis(m0_p)

#prior for poisson
prior_m0_p <- extract.prior(m0_p)
#plotting prior for null poisson
plot_prior_m0_p <- exp(prior_m0_p$a)
dens(main ="prior predictive plot for null poisson model", plot_prior_m0_p, adj = 0.1)
max(plot_prior_m0_p)
mean(plot_prior_m0_p)
min(plot_prior_m0_p)

#intercept only Gamma-poisson model
m0_g <- ulam(
  alist(
    tp ~ dgampois(lambda, phi),
    log(lambda) <- a,
    a ~ dnorm(1.5, .2),
    phi ~ dexp(1)
  ), data= dat, chains = 4, cores = 4, log_lik = TRUE
)
precis(m0_g)

#prior for null Gamma-poisson
prior_m0_g <- extract.prior(m0_g)
#plotting prior for null poisson
plot_prior_m0_g <- exp(prior_m0_g$a)
dens(main ="prior predictive plot", plot_prior_m, adj = 0.1)
max(plot_prior_m)
mean(plot_prior_m)
min(plot_prior_m)

#comparing both null models
compare(m0_p, m0_g, func = WAIC)


#interaction models for poisson model

#conditioning on Category
m1_p <- ulam(
  alist(
    tp ~ dpois(lambda),
    log(lambda) <- a + bc[Cat],
    a ~ dnorm(1.5, .2),
    bc[Cat] ~ dnorm(0, 0.5)
  ), data = dat, chains = 4, cores=4, iter = 3000, log_lik = TRUE
)
prior

precis(m1_p, depth = 2)
#conditioning on Technique
m2_p <- ulam(
  alist(
    tp ~ dpois(lambda),
    log(lambda) <- a + bt[Tech],
    a ~ dnorm(1.5, .2),
    bt[Tech] ~ dnorm(0, 0.5)
  ),data = dat, chains = 4, cores=4, iter = 3000, log_lik = TRUE, cmdstan = TRUE
)

#prior for M11.10 poisson
prior_m11.10 <- extract.prior(m11.10)
#plotting prior for null poisson
plot_prior_m11.10 <- exp(prior_m11.10$a + prior_m11.10$bt[1])
dens(main ="prior predictive plot", plot_prior_m11.10, adj = 0.1)


m2b_p <- ulam(
  alist(
    tp ~ dpois(lambda),
    log(lambda) <- a + bt[Tech],
    a ~ dnorm(1.7, .2),
    bt[Tech] ~ dnorm(0, 0.5)
  ),data = dat, chains = 4, cores
  =4, iter = 3000, log_lik = TRUE, cmdstan = TRUE
)
precis(m2_p, depth = 2)

#conditioning on Technique and Category
m3_p <- ulam(
     alist (
         tp ~ dpois(lamda),
         log(lamda) <- a + bc[Cat] + bt[Tech],
         a ~ dnorm(1.5,.2),
         bc[Cat] ~ dnorm(0,0.5),
         bt[Tech] ~ dnorm(0,0.5)
     ),data=dat,chains=4,cores=4,iter = 3000,log_lik=TRUE , cmdstan = TRUE
)

precis(m3_p, depth = 2)

comparison <- compare(m0_p,m1_p,m2_p,m3_p, func = WAIC)
compare(m0_p,m1_p,m2_p,m3_p, func = WAIC)
#add cmdstan= TRUE for final model
precis(m3_p, depth = 2)
traceplot(m2_p)

#drawing Dags
dag <- dagitty (" dag{
    TP <- T
    TP <- C
}")
coordinates (dag) <- list( x=c(TP=0,C=-2,T=2), y=c(TP=0,C=-1,T=-1) )
drawdag (dag)


#comparing techniques
c1 <- rgb(0,128,0,max = 255, alpha = 80, names = "green")
c2 <- rgb(255,0,0, max = 255, alpha = 80, names = "red")

raw_data <- hist(clean$tp, freq=FALSE, col=c1)
m2_data_list <- hist(sim(m2_p, dat), freq=FALSE, col=c2, add = TRUE)


#simulation for testing new technique and old technique
technique1 <- sim(m11.10, data = list(Technique=1))
technique2 <- sim(m11.10, data = list(Technique=2))

#difference between technique
technique_difference <- technique1 - technique2

table(sign(technique_diff))
precis(m11.10, depth = 2)

# dataframes
dataFrame <- as.data.frame(dat)
dataFrameLE <- dataFrame[dataFrame$cid == 1, ]
dataFrameME <- dataFrame[dataFrame$cid == 2, ]
dataFrameOT <- dataFrame[dataFrame$tid == 1, ]
dataFrameNT <- dataFrame[dataFrame$tid == 2, ]

m11.10_OT<- hist(sim(m11.10, dataFrameOT), freq = FALSE, col=c1)
m11.10_NT <- hist(sim(m11.10, dataFrameNT), freq = FALSE, col=c2, add = TRUE)

sim(m2_p,)