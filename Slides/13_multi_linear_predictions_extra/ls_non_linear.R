# Fitting LS and LAD lines
library(tidyverse)
library(scales)
ames_train <- read_csv("ames_train_multi.csv")
ames_train %>% print(width = Inf)




#-----------------------------------------------------------------------------#
################################ Explorations #################################
#-----------------------------------------------------------------------------#

ames_train %>% 
  select_if(is.numeric) %>%
  pivot_longer(cols = everything()) %>%
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap(~name, scales = "free")



# let's log-transform sale price, total_living area and year built
ames_train %>% 
  transmute(bedrooms = bedrooms,
            quality_score = quality_score,
            # log-transform these three variables:
            log_sale_price = log(sale_price),
            log_total_living_area = log(total_living_area),
            log_year_built = log(year_built)) %>%
  select_if(is.numeric) %>%
  pivot_longer(cols = everything()) %>%
  mutate(name = fct_inorder(name)) %>%
  ggplot() +
  geom_histogram(aes(x = value)) +
  facet_wrap(~name, scales = "free")
# it seems to be effective at symetrizing sale price and living area




# look at the relationship between sale price and other features
gg_beds_price <- ames_train %>%
  ggplot() +
  geom_boxplot(aes(x = bedrooms, y = sale_price, group = bedrooms)) 
gg_year_price <- ames_train %>%
  ggplot() +
  geom_point(aes(x = year_built, y = sale_price),
             alpha = 0.4)
gg_quality_price <- ames_train %>%
  ggplot() +
  geom_boxplot(aes(x = quality_score, y = sale_price, group = quality_score))
gg_area_price <- ames_train %>%
  ggplot() +
  geom_point(aes(x = total_living_area, y = sale_price),
             alpha = 0.4)

library(patchwork)
(gg_beds_price + gg_year_price) / (gg_quality_price + gg_area_price)




# look at the relationship between log(sale price) and other features
gg_beds_log_price <- ames_train %>%
  ggplot() +
  geom_boxplot(aes(x = bedrooms, y = log(sale_price), group = bedrooms)) 
gg_year_log_price <- ames_train %>%
  ggplot() +
  geom_point(aes(x = year_built, y = log(sale_price)),
             alpha = 0.4)
gg_quality_log_price <- ames_train %>%
  ggplot() +
  geom_boxplot(aes(x = quality_score, y = log(sale_price), group = quality_score))
gg_area_log_price <- ames_train %>%
  ggplot() +
  geom_point(aes(x = total_living_area, y = log(sale_price)),
             alpha = 0.4)

(gg_beds_log_price + gg_year_log_price) / (gg_quality_log_price + gg_area_log_price)



#-----------------------------------------------------------------------------#
############################# Performance comparison ##########################
#-----------------------------------------------------------------------------#

# untransformed fit
lm1 <- lm(sale_price ~ ., 
           data = ames_train)
pred1 <- predict(lm1, ames_test)
#rsq
cor(pred1, ames_test$sale_price)^2

# log-transformed response fit
lm2 <- lm(log(sale_price) ~ ., 
           data = ames_train)
pred2 <- exp(predict(lm2, ames_test)) # note the exponentiation
#rsq
cor(pred2, ames_test$sale_price)^2

# log-transformed predictors fit
lm3 <- lm(sale_price ~ log(total_living_area) + neighborhood + 
             year_built + bedrooms + quality_score, 
           data = ames_train)
pred3 <- predict(lm3, ames_test)
#rsq
cor(pred3, ames_test$sale_price)^2

# log-transformed response and predictors
lm4 <- lm(log(sale_price) ~ log(total_living_area) + neighborhood + 
             year_built + bedrooms + quality_score, 
           data = ames_train)
pred4 <- exp(predict(lm4, ames_test))
#rsq
cor(pred4, ames_test$sale_price)^2











#-----------------------------------------------------------------------------#
################################ Quality score example ########################
#-----------------------------------------------------------------------------#
# to produce the figures from the slides

# no transformation
lm_untransformed <- lm(sale_price ~ quality_score, ames_train)
summary(lm_untransformed)$r.squared
# plot the fit
ggplot(ames_train) +
  geom_boxplot(aes(x = as.factor(quality_score), 
                   y = sale_price)) +
  geom_abline(intercept = lm_untransformed$coef[1], 
              slope = lm_untransformed$coef[2]) +
  theme_classic() +
  scale_y_continuous("Sale Price", breaks = seq(0, 6, 2) * 100000,
                     labels = scales::dollar(seq(0, 6, 2) * 100000)) +
  scale_x_discrete("Quality score") 
# generate prediction for a house with quality score 7
predict(lm_untransformed, data.frame(quality_score = 7))



# Log transformation of response
lm_log_resp <- lm(log(sale_price) ~ quality_score, ames_train)
summary(lm_log_resp)$r.squared
fit_log_resp <- data.frame(x = 1:10,
                           y = exp(predict(lm_log_resp, 
                                           newdata = data.frame(quality_score = 1:10))))

# plot the fit
ggplot(ames_train) +
  geom_boxplot(aes(x = as.factor(quality_score), 
                   y = sale_price)) +
  geom_line(aes(x = x, y = y), 
            data = fit_log_resp) +
  theme_classic() +
  scale_y_continuous("Sale Price", breaks = seq(0, 6, 2) * 100000,
                     labels = scales::dollar(seq(0, 6, 2) * 100000)) +
  scale_x_discrete("Quality score") 
# generate prediction for a house with quality score 7
# note: you need to exponentiate the predicted response
exp(predict(lm_log_resp, data.frame(quality_score = 7)))



lm_exp_pred <- lm(sale_price ~ exp(quality_score), ames_train)
summary(lm_exp_pred)
# plot the fit
fit_exp_pred <- data.frame(x = 1:10,
                           y = predict(lm_exp_pred, 
                                       newdata = data.frame(quality_score = 1:10)))
ggplot(ames_train) +
  geom_boxplot(aes(x = as.factor(quality_score), 
                   y = sale_price)) +
  geom_line(aes(x = x, y = y), 
            data = fit_exp_pred) +
  theme_classic() +
  scale_y_continuous("Sale Price", breaks = seq(0, 6, 2) * 100000,
                     labels = scales::dollar(seq(0, 6, 2) * 100000)) +
  scale_x_discrete("Quality score") 
# generate prediction for a house with quality score 7
# note: no need to transform the predicted response
predict(lm_exp_pred, data.frame(quality_score = 7))




lm_exp_pred <- lm(sale_price ~ (quality_score), ames_train)
summary(lm_exp_pred)
# plot the fit
fit_exp_pred <- data.frame(x = 1:10,
                           y = predict(lm_exp_pred, 
                                       newdata = data.frame(quality_score = 1:10)))
ggplot(ames_train) +
  geom_boxplot(aes(x = as.factor(quality_score), 
                   y = sale_price)) +
  geom_line(aes(x = x, y = y), 
            data = fit_exp_pred) +
  theme_classic() +
  scale_y_continuous("Sale Price", breaks = seq(0, 6, 2) * 100000,
                     labels = scales::dollar(seq(0, 6, 2) * 100000)) +
  scale_x_discrete("Quality score") 
# generate prediction for a house with quality score 7
# note: no need to transform the predicted response
predict(lm_exp_pred, data.frame(quality_score = 7))


