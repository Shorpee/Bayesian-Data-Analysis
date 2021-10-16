library(rethinking)
data("cherry_blossoms")
d <- cherry_blossoms
precis(d)

m4.17 <- quap(
  alist(
    T ~ dnorm(mu , sigma) ,
    mu <- a + B %*% w , 
    a ~ dnorm(6,10),
    w ~ dnorm(0,1),
    sigma ~ dexp(1)
  ),
  data = list( T=d2$temp , B=B) , 
  start = list( w=rep(0, ncol(B)))
)
