p_grid <- seq(from=0, to=1 , length.out=1000)
prob_p <- rep(1, 1000)
prob_data <- dbinom(6, size = 9, prob = p_grid)
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)
plot(posterior ~ p_grid, type="b")

samples <- sample(p_grid, prob = posterior, size=1e4 , replace = TRUE)

plot(samples)
