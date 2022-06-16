# computing a p-value
set.seed(2916)
# randomly generate some 0/1 bernoulli data
data <- rbinom(30, 1, 0.3)
data

n <- length(data)
p_hat <- mean(data)
p_hat

# H0: p = 0.3
# H1: p != 0.3
p0 <- 0.3


# Using the normal approximation
z <- (p_hat - p0) / sqrt(p0 * (1 - p0) / n)
z
p_value <- 2 * (1 - pnorm(z))
p_value

# prop.test wants the number of successes
num_success <- sum(data)
num_success

prop.test(num_success, n, p = p0, alternative = "two.sided", correct = FALSE)
