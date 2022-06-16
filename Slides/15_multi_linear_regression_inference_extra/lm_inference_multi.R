library(tidyverse)

ames_train <- read_csv("ames_train_multi.csv") %>%
  select(-neighborhood)
ames_train %>% print(width = Inf)

# For the toy example in the slides, we use only the first 10 rows
#ames_train <- ames_train[1:10, ]

# Get 
X <- ames_train %>%
  mutate(int = 1) %>%
  select(int, living_area = total_living_area, quality_score, year_built, bedrooms) %>%
  as.matrix()
X
Y <- ames_train[, 1] %>%
  as.matrix
Y

n <- length(Y)
n
p <- ncol(X)
p

XtX_inv <- solve(t(X) %*% X)
beta <- XtX_inv %*% t(X) %*% Y
beta

# compute predictions
Yhat <- X %*% beta 
Yhat

# compute residuals
r <- Y - Yhat
r

# two ways of computing sigma2
sigma2 <- t(r) %*% (r) / (n-p)
sigma2
sigma2 <- sum(r^2) / (n-p)
sigma2

# compute the SD of beta_hat
sigma2_beta <- sigma2 * diag(XtX_inv)
sigma2_beta

# standardized coefficients
t <- beta / sqrt(sigma2_beta)
t


# compute p-value for each beta0
p <- 2*(1 - pt(abs(t), n-p))
p


# note that these values match what we get from summary(lm())
ls_fit <- lm(sale_price ~ ., ames_train)
summary(ls_fit)




