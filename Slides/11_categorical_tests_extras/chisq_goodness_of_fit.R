# chi-squared goodness of fit test
library(tidyverse)

# create the data
bacteria <- 
  data.frame(value = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 19),
             obs = c(56, 104, 80, 62, 42, 27, 9, 9, 5, 3, 2, 1))
bacteria


# estimate lambda from the mean value (each value occurs "obs" times)
lambda_est <- sum(bacteria$value * bacteria$obs) / 400
lambda_est

# plot the distribution and our Poisson fit
x_seq <- 0:19
pois_fit <- data.frame(x = x_seq, 
                       fit = dpois(x_seq, lambda_est))
ggplot(bacteria) +
  geom_col(aes(x = value, y = obs / 400)) +
  geom_line(aes(x = x, y = fit), data = pois_fit) +
  geom_point(aes(x = x, y = fit), data = pois_fit) +
  theme_classic() +
  labs(y = "Density")


# compute the expected values 
bacteria <- bacteria %>%
  mutate(expected = 400 * dpois(value, lambda_est))
bacteria

# Based on the rule that there can be no expected value cell less than 5,
# we need to aggregate the final 5 cells
bacteria_trimmed <- bacteria[1:7, ]
bacteria_trimmed[8, "obs"] <- sum(bacteria[8:12, "obs"])
bacteria_trimmed[8, "expected"] <- sum(bacteria[8:12, "expected"])
bacteria_trimmed[8, "value"] <- 7
bacteria_trimmed

# compute the test statistic
bacteria_trimmed <- bacteria_trimmed %>%
  mutate(x2 = (obs - expected)^2 / expected)
bacteria_trimmed

x2 <- sum(bacteria_trimmed$x2)
x2

# pvalue
n <- nrow(bacteria_trimmed)
c <- 1 # estimated lambda
df <-  n - c - 1
df
# P(X2 >= x2)
1 - pchisq(x2, n)

# chisq.test(x = bacteria_trimmed$obs, 
#            p = dpois(bacteria_trimmed$value, lambda_est))


#-----------------------------------------------------------------------------#
############################## Mendel's example ###############################
#-----------------------------------------------------------------------------#


# create the data
mendel <- 
  data.frame(value = c("smooth yellow", "smooth green", "wrinkled yellow", "wrinkled green"),
             obs = c(315, 108, 102, 31))
mendel

theoretical <- 
  data.frame(value = c("smooth yellow", "smooth green", "wrinkled yellow", "wrinkled green"),
             p = c(9/16, 3/16, 3/16, 1/16)) 
theoretical



# compute the expected values
N <- sum(mendel$obs)
N
mendel <- mendel %>%
  mutate(expected = N * theoretical$p)
mendel

# compute the test statistic
mendel <- mendel %>%
  mutate(x2 = (obs - expected)^2 / expected)
mendel

x2 <- sum(mendel$x2)
x2

# pvalue
df <- 4 - 0 - 1
df
round(1 - pchisq(x2, df), 3)

# when your X's have a finite set of options, 
# you can use chisq.test
chisq.test(x = mendel$obs, p = theoretical$p)
