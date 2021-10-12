library (rethinking)

m <- ulam (
  alist ( W ~ dbinom(3, p),
          p ~ dunif(0, 1)
  ), data=list(W=c(3)), cores=4, chains=4
)

m
