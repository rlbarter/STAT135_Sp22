# Code for computing bootstrap estimates of the bias and variance of the 
# sample mean for a toy data example
# Author: Rebecca Barter

# Note: if you've never seen `map_` functions before, 
# check out my blog post on purrr: 
# https://www.rebeccabarter.com/blog/2019-08-19_purrr/
# map functions are used for iterating (they're like `apply()` functions)


library(tidyverse)

# our data sample contains 8 data points
sample_data <- c(6, 1, 4, 9, 2, 0, 1, 3)
set.seed(238479)



############### Non-parametric bootstrap ##################


# draw some non-parametric bootstrap samples
np_boot_mean_df <- map_df(1:100, function(i) {
  # sample from the data with replacement
  bootstrap_data <- sample(sample_data, length(sample_data), replace = TRUE)
  # compute the sample mean of the bootstrap sample
  data.frame(boot_mean = mean(bootstrap_data))
})

# np bootstrap estimate of the sample mean bias: 
np_bias <- mean(np_boot_mean_df$boot_mean) - mean(sample_data)
np_bias
# np bootstrap estimate of the sample mean SD
np_var <- var(np_boot_mean_df$boot_mean)
np_var

# view a histogram of the bootstrapped sample means
np_boot_mean_df %>%
  ggplot() +
  geom_histogram(aes(x = boot_mean), color = "white",
                 binwidth = 0.5) +
  # add a line for the sample estimate of the mean
  geom_vline(xintercept = mean(sample_data), 
             color = "orange", size = 2) +
  # add a line for the bootstrapped estimate of the mean
  geom_vline(xintercept = mean(np_boot_mean_df$boot_mean), 
             color = "cornflowerblue", size = 2) +
  labs(x = "bootstrapped sample mean") +
  theme_classic()



############### Parametric bootstrap ##################

# draw some parametric bootstrap samples
p_boot_mean_df <- map_df(1:100, function(i) {
  # sample from the approximated distribution
  bootstrap_data <- rnorm(length(sample_data), mean(sample_data), sd(sample_data))
  # compute the sample mean of the parametric bootstrap sample
  data.frame(boot_mean = mean(bootstrap_data))
})

# parametric bootstrap estimate of the sample mean bias: 
p_bias <- mean(p_boot_mean_df$boot_mean) - mean(sample_data)
p_bias
# parametric bootstrap estimate of the sample mean SD
p_var <- var(p_boot_mean_df$boot_mean)
p_var


# view a histogram of the bootstrapped sample means
p_boot_mean_df %>%
  ggplot() +
  geom_histogram(aes(x = boot_mean), color = "white",
                 binwidth = 0.5) +
  # add a line for the sample estimate of the mean
  geom_vline(xintercept = mean(sample_data), 
             color = "orange", size = 2) +
  # add a line for the bootstrapped estimate of the mean
  geom_vline(xintercept = mean(p_boot_mean_df$boot_mean), 
             color = "cornflowerblue", size = 2) +
  labs(x = "bootstrapped sample mean") +
  theme_classic()

