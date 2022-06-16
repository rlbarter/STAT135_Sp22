# 95% CI
alpha <- 0.05
# alpha = 0.05, alpha/2 = 0.025
round(qnorm(0.025), 2)
round(qnorm(0.975), 2)

# 90% CI
alpha <- 0.1
# alpha = 0.1, alpha/2 = 0.05
round(qnorm(0.05), 2)
round(qnorm(0.95), 2)

# 99% CI
alpha <- 0.01
# alpha = 0.01, alpha/2 = 0.005
round(qnorm(0.005), 2)
round(qnorm(0.995), 2)

