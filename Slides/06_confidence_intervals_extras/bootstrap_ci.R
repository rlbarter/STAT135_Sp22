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
# Choose number of bootstrap samples to draw
N <- 500

#------------------------------------------------------------------------------
############ Simulate a dataset from a Normal distribution ####################
#------------------------------------------------------------------------------
# draw our original sample from the "population"
set.seed(94632)
sample_data <- rnorm(n, mu, sigma)


#------------------------------------------------------------------------------
########### Compute a 95% non-parametric boostrap CI ##########################
#------------------------------------------------------------------------------
# draw N non-parametric bootstrap samples from our sample
samples_boot <- map(1:N, 
                    ~{sample(sample_data, replace = TRUE)})

samples_boot <- map(1:N, function(x) {
  sample(sample_data, replace = TRUE)
})

# calculate the (bootstrap) mean for each bootstrap sample
boot_means <- map_dbl(samples_boot, ~mean(.))
boot_means <- map_dbl(samples_boot, function(x) { mean(x) })

data.frame(boot_mean = boot_means) %>%
  ggplot() +
  geom_histogram(aes(x = boot_mean))
  

# compute a 95% CI from these bootstrapped means
boot_ci <- c(quantile(boot_means, alpha/2), 
             quantile(boot_means, (1 - alpha/2)))
boot_ci




#------------------------------------------------------------------------------
#### Repeat this entire process many times to simulate bootstrap coverage #####
#------------------------------------------------------------------------------

# Repeat this entire process 100 more times
many_boot_confidence_intervals <- map_df(1:100, ~{
  # draw a sample from the population
  .sample_data <- rnorm(n, mu, sigma)
  # draw bootstrapped samples from the sample
  .samples_boot <- map(1:N, ~sample(.sample_data, replace = TRUE))
  # compute the mean of each bootstrapped sample
  .boot_means <- map_dbl(.samples_boot, ~mean(.))
  # create a CI
  boot_ci <- tibble(left = quantile(.boot_means, alpha/2), 
                    right = quantile(.boot_means, (1-alpha/2)))
  return(boot_ci)
})

# Estimate the coverage of our CI, i.e., compute the prop of CIs that contain mu
coverage_est <- many_boot_confidence_intervals %>%
  mutate(contains_mu = (left <= mu) & (right >= mu)) %>%
  summarize(sum(contains_mu) / n())
# report the estimated coverage
coverage_est

many_boot_confidence_intervals %>%
  mutate(contains_mu = (left <= mu) & (right >= mu)) %>%
  mutate(id = 1:n()) %>%
  ggplot() + 
  geom_segment(aes(x = left, xend = right, y = id, yend = id, color = contains_mu)) +
  geom_vline(xintercept = mu) +
  scale_color_manual(values = c("red", "grey50")) +
  scale_x_continuous(limits = c(-1.2, 1.2)) +
  theme_classic()

