# computing a p-value
set.seed(291016)
data <- round(rnorm(20, 5.4, 2))

# H0: mu = 5
# H1: mu > 5

mu_0 <- 5
sd <- 2
n <- length(data)
x_bar <- mean(data)



# to test that the mean is greater than 5
z <- (x_bar - mu_0) / (sd / sqrt(n))
z
1 - pnorm(z)

