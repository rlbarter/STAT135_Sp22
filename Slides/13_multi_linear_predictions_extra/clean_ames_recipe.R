# Preparing the Ames data for predictive modeling
library(tidyverse)
library(readxl)
library(recipes)
library(fastDummies)
library(janitor)
ames_orig <- read_xls("AmesHousing.xls")
head(ames_orig) %>% print(width = Inf)


set.seed(782264)
ames <- ames_orig %>%
  clean_names()
# create 70% training set
ames_train <- ames %>% sample_frac(0.7)
# create 70% test set
ames_test <- ames %>% filter(!(pid %in% ames_train$pid))
ames_train <- ames_train %>% select(-pid)
ames_test <- ames_test %>% select(-pid)


model_recipe <- recipe(sale_price ~ ., 
                       data = ames_train)

model_recipe_steps <- model_recipe %>% 
  # convert the additional ingredients variable to dummy variables
  step_impute_mean(all_numeric()) %>%
  step_impute_mode(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  # remove predictor variables that are almost the same for every entry
  step_nzv(all_predictors()) 

prepped_recipe <- prep(model_recipe_steps, 
                       training = ames_train)

ames_train_clean <- bake(prepped_recipe, ames_train) 
ames_test_clean <- bake(prepped_recipe, ames_test)  %>%
  drop_na()


lm_fit <- lm(sale_price ~ ., ames_train_clean)
pred <- predict(lm_fit, ames_test_clean)
cor(pred, ames_test_clean$sale_price)
