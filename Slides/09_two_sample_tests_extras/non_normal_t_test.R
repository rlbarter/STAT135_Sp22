# computing a p-value
set.seed(291016)
data <- rexp(20, 0.5) # true mean is 2

mu_0 <- 2
n <- length(data)
x_bar <- mean(data)
# the SD is *computed* from the data!
sd <- sd(data)

# H0: mu = 5
# H1: mu > 5

# manually test that the mean is greater than 5
t <- (x_bar - mu_0) / (sd / sqrt(n))
t
1 - pt(t, df = n-1)



pvals <- map_df(1:10000, ~{
  .data <- rexp(20, 0.5) # true mean is 2
  

  .x_bar <- mean(.data)
  # the SD is *computed* from the data!
  .sd <- sd(.data)
  
  # H0: mu = 5
  # H1: mu > 5
  
  # manually test that the mean is greater than 5
  .t <- (.x_bar - mu_0) / (.sd / sqrt(n))
  p_val <- 1 - pt(.t, df = n-1)
  return(data.frame(p_val = p_val))
})

# this should be 0.05!
pvals %>% 
  summarise(sum(p_val < 0.05) / n())


################


pvals <- map_df(1:10000, ~{
  .data <- rnorm(20, 2, 2)  # true mean is 2
  
  
  .x_bar <- mean(.data)
  # the SD is *computed* from the data!
  .sd <- sd(.data)
  
  # H0: mu = 5
  # H1: mu > 5
  
  # manually test that the mean is greater than 5
  .t <- (.x_bar - mu_0) / (.sd / sqrt(n))
  p_val <- 1 - pt(.t, df = n-1)
  return(data.frame(p_val = p_val))
})

# this should be 0.05!
pvals %>% 
  summarise(sum(p_val < 0.05) / n())
