# computing a p-value for a t-test
set.seed(291016)
data <- round(rnorm(20, 2, 2))

mu_0 <- 2
n <- length(data)
x_bar <- mean(data)
# the SD is *computed* from the data!
sd <- sd(data)

# H0: mu = 2
# H1: mu > 2

# manually test that the mean is greater than 2
t <- (x_bar - mu_0) / (sd / sqrt(n))
t
1 - pt(t, df = n-1)


# use t.test to test that the mean is greater than 2
t.test(data, mu = mu_0, alternative = "greater")



# --------------------------------------------------------------------------- #
# What proportion of the intervals reject the null when the null is true? #####
# --------------------------------------------------------------------------- #


pvals <- map_df(1:10000, ~{
  .data <- rnorm(20, 2, 2)  # true mean is 2
  
  
  .x_bar <- mean(.data)
  # the SD is *computed* from the data!
  .sd <- sd(.data)
  
  # H0: mu = 2
  # H1: mu > 2
  
  # manually test that the mean is greater than 2
  .t <- (.x_bar - mu_0) / (.sd / sqrt(n))
  p_val <- 1 - pt(.t, df = n-1)
  return(data.frame(p_val = p_val))
})

# this should be 0.05
pvals %>% 
  summarise(sum(p_val < 0.05) / n())





# --------------------------------------------------------------------------- #
## What about if the data is non-normal? ######################################
# --------------------------------------------------------------------------- #



pvals <- map_df(1:10000, ~{
  .data <- rexp(20, 0.5)  # true mean is 2
  
  
  .x_bar <- mean(.data)
  # the SD is *computed* from the data!
  .sd <- sd(.data)
  
  # H0: mu = 2
  # H1: mu > 2
  
  # manually test that the mean is greater than 2
  .t <- (.x_bar - mu_0) / (.sd / sqrt(n))
  p_val <- 1 - pt(.t, df = n-1)
  return(data.frame(p_val = p_val))
})

# This is much lower than 0.05!!!!!
pvals %>% 
  summarise(sum(p_val < 0.05) / n())
### This is because the t-test assumes that your data is normally distributed!!