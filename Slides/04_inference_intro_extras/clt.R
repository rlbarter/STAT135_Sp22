library(tidyverse)
set.seed(29874)
n <- 500
# draw 1000 samples of size 500 and compute their mean
# The ~{} notation is to create an "anonymous" function
gamma_samples <- purrr::map(1:1000, ~{rgamma(n, 2, 0.5)})

# compute the theoretical density function to plot over the histogram
gamma_density <- data.frame(x = seq(0.1, 20, 0.1),
                            y = dgamma(seq(0.1, 20, 0.1), 2, 0.5))
  
  
# plot the distribution of one of the samples
data.frame(gamma_sample = gamma_samples[[1]]) %>%
  ggplot() +
  geom_histogram(aes(x = gamma_sample, y = ..density..),
                 col = "white", binwidth = 1) +
  geom_line(aes(x = x, y = y), data = gamma_density) +
  theme_classic()
# the mean of this sample is
mean(gamma_samples[[1]])


# compute the means
gamma_sample_means <- map_df(gamma_samples, function(.sample) {
  # compute the mean of each sample and put it in a df
  data.frame(mean_gamma = mean(.sample))
})

# compute the theoretical mean and variance of the sample mean
# to use for plotting the theoretical normal distribution
theoretical_mean <- 2 / 0.5
theoretical_var <- 2 / (n * 0.5^2)
normal_density <- data.frame(x = seq(3.5, 4.5, 0.01),
                            y = dnorm(seq(3.5, 4.5, 0.01), 
                                      theoretical_mean, sqrt(theoretical_var)))
  

# plot the distribution of the means
gamma_sample_means %>%
  ggplot() +
  geom_histogram(aes(x = mean_gamma, y = ..density..),
                 col = "white", binwidth = 0.05) +
  geom_line(aes(x = x, y = y), 
            data = normal_density) +
  theme_classic()
