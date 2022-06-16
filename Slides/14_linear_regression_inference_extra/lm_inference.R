library(tidyverse)
library(scales)
ames_train <- read_csv("ames_train.csv")
ames_train %>% print(width = Inf)

# For the toy example in the slides, we use only the first 10 rows
ames_train <- ames_train[1:10, ]


x <- ames_train$total_living_area
x
y <- ames_train$sale_price
y
n <- length(y)

# estimate coefficients using LS
b1 <- cov(x, y) / var(x)
b1
# alternatively
b1 <- cor(x, y) * sd(y) / sd(x)
b1
b0 <- mean(y) - b1 * mean(x)
b0

# compute predictions
yhat <- b0 + b1 * x

# compute residuals
r <- y - yhat
# estimate sigma2
sigma2_est <- sum(r^2) / (n-2)

# compute SD of beta0
sigma_b0_est <- sqrt(sigma2_est * sum(x^2) / (n * sum(x^2) - sum(x)^2))
sigma_b0_est
# compute SD of beta1
sigma_b1_est <- sqrt(n * sigma2_est / (n * sum(x^2) - sum(x)^2))
sigma_b1_est

# compute test statistic of beta0
t_b0 <- b0 / sigma_b0_est
t_b0
# compute test statistic of beta1
t_b1 <- b1 / sigma_b1_est
t_b1

# compute p-value for beta0
p_b0 <- 2*(1 - pt(abs(t_b0), n-2))
p_b0
# compute p-value for beta1
p_b1 <- 2*(1 - pt(abs(t_b1), n-2))
p_b1



# note that these values match what we get from summary(lm())
ls_fit <- lm(sale_price ~ total_living_area, ames_train)
summary(ls_fit)



#-----------------------------------------------------------------------------#
################# residual plot ###############################################
#-----------------------------------------------------------------------------#

data.frame(residuals = r, yhat = yhat) %>%
  ggplot() +
  geom_point(aes(x = yhat, y = residuals),
             alpha = 0.5) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_x_continuous(breaks = 1:5 * 1e05,
                     labels = comma(1:5 * 1e05)) 
  # scale_y_continuous(breaks = -2:3 * 1e05,
  #                    labels = comma(-2:3 * 1e05))





# recreate residual plot for multiple regression
ames_train_multi <- read_csv("ames_train_multi.csv")
ls_fit_multi <- lm(sale_price ~ total_living_area + quality_score + 
                     year_built + bedrooms, ames_train_multi)
summary(ls_fit_multi)

# residual plot
data.frame(residuals = ls_fit_multi$residuals,
           yhat = ls_fit_multi$fitted) %>%
  ggplot() +
  geom_point(aes(x = yhat, y = residuals),
             alpha = 0.5) +
  geom_hline(yintercept = 0) +
  theme_classic() +
  scale_x_continuous(breaks = 1:5 * 1e05,
                     labels = comma(1:5 * 1e05)) +
  scale_y_continuous(breaks = -2:3 * 1e05,
                     labels = comma(-2:3 * 1e05))

