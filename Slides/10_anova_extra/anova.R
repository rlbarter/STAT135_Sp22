# ANOVA
library(tidyverse)

pizza <- data.frame(la_vals = c(32, 48, 51, 47, 41, 35, 37, 41, 43, 38),
                    sliver = c(27, 32, 37, 21, 25, 28, 36, 38, 29, 30),
                    artichoke = c(52, 40, 41, 36, 32, 38, 37, 42, 39, 30))
pizza

pizza_long <- pivot_longer(pizza, cols = everything(), 
                           names_to = "restaurant", values_to = "time")
pizza_long


# EDA: do the means of each group *look* different?
pizza_long %>%
  ggplot() +
  geom_boxplot(aes(x = restaurant, y = time)) +
  geom_hline(yintercept = mean(pizza_long$time), col = "orange") +
  theme_classic() +
  labs(y = "Delivery time", x = "Pizza place")


# Do the samples potentially have common variance?
pizza_long %>%
  group_by(restaurant) %>%
  summarise(sd = sd(time))


# Do the samples look normal?
pizza_long %>%
  ggplot() +
  geom_qq(aes(sample = time)) +
  geom_qq_line(aes(sample = time)) +
  facet_wrap(~restaurant) +
  theme_bw()


# Compute the things we need to do the test
I <- ncol(pizza)
I
J <- nrow(pizza)
J

dfb <- I - 1
dfw <- I * (J - 1)
dft <- I * J - 1

ssb <- pizza_long %>%
  group_by(restaurant) %>%
  summarise(group_mean = mean(time)) %>%
  ungroup() %>%
  mutate(global_mean = mean(group_mean)) %>%
  summarise(ssb = J * sum((group_mean - global_mean)^2)) %>%
  pull(ssb)
ssb

ssw <- pizza_long %>%
  group_by(restaurant) %>%
  # add a column with the group mean
  mutate(group_mean = mean(time)) %>%
  ungroup() %>%
  # compute the squared difference for each obs and its group mean
  mutate(squared_diff = (time - group_mean)^2) %>%
  # add up the squared differences
  summarise(ssw = sum(squared_diff)) %>%
  pull(ssw)
ssw

sst <- pizza_long %>%
  # add a column with the global mean
  mutate(global_mean = mean(time)) %>%
  # compute the squared difference for each obs and the global mean
  mutate(squared_diff = (time - global_mean)^2) %>%
  # add up the squared differences
  summarise(sst = sum(squared_diff)) %>%
  pull(sst)
sst


# compute MSS
msb <- ssb / dfb
msb
msw <- ssw / dfw
msw

# compute F
f <- msb / msw
f

# compute p-value
1 - pf(f, 2, 27)


# using the aov() function
anova <- aov(time ~ restaurant, data = pizza_long)
anova
summary(anova)



#-----------------------------------------------------------------------------#
############################# Kruskal-Wallis test #############################
#-----------------------------------------------------------------------------#


pizza_long <- pizza_long %>%
  mutate(ranks = rank(time))
pizza_long

# compute test statistic
ranks_group_means <- pizza_long %>%
  group_by(restaurant) %>%
  summarise(mean_rank = mean(ranks)) %>%
  pull(mean_rank)
ranks_group_means

# test statistic
K <- 12 / (I*J * (I*J + 1)) * (J * sum(ranks_group_means^2)) - 3 * (I*J + 1)
K

# compute p-value
1 - pchisq(K, I-1)


# using kruskal.test()
kruskal.test(time ~ restaurant, pizza_long)
# I think the slight difference in test statistic and p-values are based on 
# an adjustment for ties that I am ignoring in the manual calculation