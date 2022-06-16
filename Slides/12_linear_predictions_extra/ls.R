# Fitting LS and LAD lines
library(tidyverse)

ames <- read_csv("ames_train.csv")
ames

y <- ames$sale_price
x <- ames$total_living_area


# manually fit a LS line
b1 <- cov(x, y) / var(x)
b1
# alternatively
b1 <- cor(x, y) * sd(y) / sd(x)
b1
b0 <- mean(y) - b1 * mean(x)
b0

# or use lm
ames_ls <- lm(sale_price ~ total_living_area, data = ames)
summary(ames_ls)


# predict the sale price of a 2000ft2 home using LS:
predict(ames_ls, 
        newdata = data.frame(total_living_area = 2000))
# compute manually:
b0 + b1 * 2000





# compute the LAD fitted line
library(L1pack)
ames_lad <- lad(sale_price ~ total_living_area, data = ames)
ames_lad

# predict the sale price of a 2000ft2 home using LAD:
predict(ames_lad, 
        newdata = data.frame(total_living_area = 2000))






##---------------------------------------------------------------------------##
####################### Evaluating predictions ################################
##---------------------------------------------------------------------------##

# Create a data frame with the observed and predicted prices
ames_train_pred <- data.frame(sale_price = ames$sale_price,
                              pred_sale_price_ls = predict(ames_ls, newdata = ames),
                              pred_sale_price_lad = predict(ames_lad, newdata = ames))
head(ames_train_pred)


# MSE computation
# Note LS is better in terms of MSE
ames_train_pred %>% 
  summarise(mse_ls = mean((sale_price - pred_sale_price_ls)^2),
            mse_lad = mean((sale_price - pred_sale_price_lad)^2))


# rMSE computation
# Note LS is better in terms of MSE
ames_train_pred %>% 
  summarise(rmse_ls = sqrt(mean((sale_price - pred_sale_price_ls)^2)),
            rmse_lad = sqrt(mean((sale_price - pred_sale_price_lad)^2)))

# MAD computation
# note LAD is better in terms of MAD
ames_train_pred %>% 
  summarise(mad_ls = mean(abs(sale_price - pred_sale_price_ls)),
            mad_lad = mean(abs(sale_price - pred_sale_price_lad)))


# correlation computation
ames_train_pred %>% 
  summarise(cor_ls = cor(sale_price, pred_sale_price_ls),
            cor_lad = cor(sale_price, pred_sale_price_lad))

# plot the observed against the predicted values
ames_train_pred %>%
  rename(ls = pred_sale_price_ls,
         lad = pred_sale_price_lad) %>%
  pivot_longer(cols = c("ls", "lad"), 
               names_to = "algorithm", values_to = "pred") %>%
  ggplot() +
  geom_point(aes(x = sale_price, y = pred), alpha = 0.3, col = "grey20") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  facet_wrap(~algorithm) +
  # extra layers to make the plot look nicer
  labs(x = "Observed sale price", y = "Predicted sale price") +
  scale_x_continuous(breaks = c(0, 2, 4, 6) * 100000, 
                     labels = scales::dollar(c(0, 2, 4, 6) * 100000)) +
  scale_y_continuous(breaks = 0:5 * 100000, 
                     labels = scales::dollar(0:5 * 100000)) 

# R-squared computation
# the performance of the two algorithms is the same in terms of Rsq
ames_train_pred %>% 
  summarise(rsq_ls = cor(sale_price, pred_sale_price_ls)^2,
            rsq_lad = cor(sale_price, pred_sale_price_lad)^2)
# from the formula on the slides
var_y <- var(ames_train_pred$sale_price)
var_e <- var(ames_train_pred$sale_price - ames_train_pred$pred_sale_price_ls)
(var_y - var_e) / var_y





#-----------------------------------------------------------------------------#
######################## Evaluating on the test set ##########################
#-----------------------------------------------------------------------------#

ames_test <- read_csv("ames_test.csv")
dim(ames_test)
head(ames_test)

# compute the predictions for the test set
ames_test_pred <- data.frame(sale_price = ames_test$sale_price,
                             pred_sale_price_ls = predict(ames_ls, newdata = ames_test),
                              pred_sale_price_lad = predict(ames_lad, newdata = ames_test))
head(ames_test_pred)





# MSE computation
# Note LS is better in terms of MSE
ames_test_pred %>% 
  summarise(mse_ls = mean((sale_price - pred_sale_price_ls)^2),
            mse_lad = mean((sale_price - pred_sale_price_lad)^2))


# rMSE computation
# Note LS is better in terms of MSE
ames_test_pred %>% 
  summarise(rmse_ls = sqrt(mean((sale_price - pred_sale_price_ls)^2)),
            rmse_lad = sqrt(mean((sale_price - pred_sale_price_lad)^2)))

# MAD computation
# note LAD is better in terms of MAD
ames_test_pred %>% 
  summarise(mad_ls = mean(abs(sale_price - pred_sale_price_ls)),
            mad_lad = mean(abs(sale_price - pred_sale_price_lad)))


# correlation computation
ames_test_pred %>% 
  summarise(cor_ls = cor(sale_price, pred_sale_price_ls),
            cor_lad = cor(sale_price, pred_sale_price_lad))

# plot the observed against the predicted values
ames_test_pred %>%
  rename(ls = pred_sale_price_ls,
         lad = pred_sale_price_lad) %>%
  pivot_longer(cols = c("ls", "lad"), 
               names_to = "algorithm", values_to = "pred") %>%
  ggplot() +
  geom_point(aes(x = sale_price, y = pred), alpha = 0.3, col = "grey20") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  facet_wrap(~algorithm) +
  # extra layers to make the plot look nicer
  labs(x = "Observed sale price", y = "Predicted sale price") +
  scale_x_continuous(breaks = c(0, 2, 4, 6) * 100000, 
                     labels = scales::dollar(c(0, 2, 4, 6) * 100000)) +
  scale_y_continuous(breaks = 0:5 * 100000, 
                     labels = scales::dollar(0:5 * 100000)) 

# R-squared computation
# the performance of the two algorithms is the same in terms of Rsq
ames_test_pred %>% 
  summarise(rsq_ls = cor(sale_price, pred_sale_price_ls)^2,
            rsq_lad = cor(sale_price, pred_sale_price_lad)^2)
