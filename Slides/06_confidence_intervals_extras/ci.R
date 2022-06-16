# Empirical evidence that a 95% bootstrap CI has 95% coverage
# Author: Rebecca Barter

library(tidyverse)

# Choose sample size
n <- 20
# Choose confidence level
alpha <- 0.05 #95% CI
# Choose population mean
mu <- 0
# Choose population sd
sigma <- 1.1


#------------------------------------------------------------------------------
############ Simulate a dataset from a Normal distribution ####################
#------------------------------------------------------------------------------
# draw our original sample from the "population"
set.seed(94632)
sample_data <- rnorm(n, mu, sigma)


#------------------------------------------------------------------------------
################## Compute a 95% theoretical CI ##############################
#------------------------------------------------------------------------------


# assume we don't know the true population variance
mu_est <- mean(sample_data)
sigma_est <- sd(sample_data)
t <- qt(1 - alpha/2, n-1)
theoretical_ci <- c(mu_est - t * sigma_est / sqrt(n), 
                    mu_est + t * sigma_est / sqrt(n))
theoretical_ci


# confirm that it agrees with the inbuilt function
# I'll explain this t.test function when we cover hypothesis tests
t.test(sample_data)$conf.int


#------------------------------------------------------------------------------
################## Estimate coverage ##############################
#------------------------------------------------------------------------------


# Repeat this entire process 100 more times
many_confidence_intervals <- map_df(1:100, ~{
  # draw a sample from the population
  .sample_data <- rnorm(n, mu, sigma)
  # compute the mean and sd of our sample
  .mu_est <- mean(.sample_data)
  .sigma_est <- sd(.sample_data)
  theoretical_ci <- tibble(left = .mu_est - t * .sigma_est / sqrt(n), 
                           right = .mu_est + t * .sigma_est / sqrt(n))
  return(theoretical_ci)
})

# Estimate the coverage of our CI, i.e., compute the prop of CIs that contain mu
coverage_est <- many_confidence_intervals %>%
  mutate(contains_mu = (left <= mu) & (right >= mu)) %>%
  summarize(sum(contains_mu) / nrow(many_confidence_intervals))
# report the estimated coverage
coverage_est

many_confidence_intervals %>%
  mutate(contains_mu = (left <= mu) & (right >= mu)) %>%
  mutate(id = 1:n()) %>%
  ggplot() + 
  geom_segment(aes(x = left, xend = right, y = id, yend = id, color = contains_mu)) +
  geom_vline(xintercept = mu) +
  scale_color_manual(values = c("red", "grey50")) +
  scale_x_continuous(limits = c(-1.5, 1.5)) +
  theme_classic()

