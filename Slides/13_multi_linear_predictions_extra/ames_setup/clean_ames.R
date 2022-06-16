# Preparing the Ames data for predictive modeling
library(tidyverse)
library(readxl)
library(fastDummies)
ames_orig <- read_xls("AmesHousing.xls")
head(ames_orig) %>% print(width = Inf)


#-----------------------------------------------------------------------------#
######################## Clean variable names #################################
#-----------------------------------------------------------------------------#

library(janitor)
ames <- ames_orig %>%
  clean_names
head(ames)


#-----------------------------------------------------------------------------#
######################## Filter out irrelevant data ###########################
#-----------------------------------------------------------------------------#


ames <- ames %>%
  filter(sale_condition == "Normal",
         # remove agricultural, commercial and industrial
         !(`ms_zoning` %in% c("A (agr)", "C (all)", "I (all)")))

# remove these variables
ames <- ames %>%
  select(-sale_condition)


#-----------------------------------------------------------------------------#
######################## explore missing values ###############################
#-----------------------------------------------------------------------------#

# count the number of missing values in each column
ames %>%
  map_dbl(~sum(is.na(.))) %>%
  sort




# let's replace each variable with the mean (numeric) or mode (cat) value
ames %>% count(bsmt_fin_type_2)
ames <- ames %>%
  mutate(bsmt_fin_type_2 = replace_na(bsmt_fin_type_2, "Unf"))

ames %>% count(electrical)
ames <- ames %>%
  mutate(electrical = replace_na(electrical, "SBrkr"))

ames %>% count(bsmt_full_bath)
ames <- ames %>%
  mutate(bsmt_full_bath = replace_na(bsmt_full_bath, 0))

ames %>% count(bsmt_half_bath)
ames <- ames %>%
  mutate(bsmt_half_bath = replace_na(bsmt_half_bath, 0))

ames %>% count(garage_finish)
ames <- ames %>%
  mutate(garage_finish = replace_na(garage_finish, "Unf"))

ames %>% count(bsmt_exposure)
ames <- ames %>%
  mutate(bsmt_exposure = replace_na(bsmt_exposure, "No"))

ames %>% count(mas_vnr_type)
ames <- ames %>%
  mutate(mas_vnr_type = replace_na(mas_vnr_type, "None"))

ames %>% count(mas_vnr_area)
ames <- ames %>%
  mutate(mas_vnr_area = replace_na(mas_vnr_area, 0))







# Example of need for human-in-the-loop:
ames %>% count(garage_yr_blt) %>% as.data.frame
# replace with mean, or replace with year the house was built?
ames <- ames %>%
  mutate(garage_yr_blt = if_else(is.na(garage_yr_blt), 
                                 year_built, 
                                 garage_yr_blt))


ames %>% count(lot_frontage) %>% as.data.frame
# fancy idea: use lot_area to predict the missing lot_frontage
ggplot(ames) +
  geom_point(aes(x = lot_area, y = lot_frontage))
# let's go simple and just mean-impute
ames <- ames %>%
  mutate(lot_frontage = if_else(is.na(lot_frontage), 
                                mean(lot_frontage, na.rm = T),
                                lot_frontage))




# check that we got all of the missing values
ames %>%
  map_dbl(~sum(is.na(.))) %>%
  sort


#-----------------------------------------------------------------------------#
#################### Handling categorical variables ###########################
#-----------------------------------------------------------------------------#


# consider the bsmt_qual variable
ames %>%
  count(bsmt_qual)
# how is a computer going to interpret these levels? ..... alphabetically? 
# Should we create dummy variables? This will remove order information
# what should we do with the "NA" (no basement)?

# can we simplify variables with many unnecessary levels?
ames %>%
  count(roof_style)

# are all of these categorical variables actually necessary?
ames %>% count(condition_2)



#-----------------------------------------------------------------------------#
#################### Creating clean data ###########################
#-----------------------------------------------------------------------------#



set.seed(782264)
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

ames_train_preprocessed <- bake(prepped_recipe, ames_train) 
ames_test_preprocessed <- bake(prepped_recipe, ames_test) 



lm_clean <- lm(sale_price ~ ., data = ames_train_preprocessed)
summary(lm_clean)$r.squared
test_pred_clean <- predict(lm_clean, ames_test_preprocessed)
cor(test_pred_clean, 
    ames_test$sale_price)^2


#-----------------------------------------------------------------------------#
######################## Write train/test data ################################
#-----------------------------------------------------------------------------#



# Note: if you want to be really careful to keep your training and testing sets 
# independent, you should generate your cleaning procedure using the 
# training data only, and write a function that you can then apply to your test data


# write data
write_csv(ames_train_preprocessed, "ames_train_full.csv")
write_csv(ames_test_preprocessed, "ames_test_full.csv")




#-----------------------------------------------------------------------------#
############# Comparing performance of cleaned vs raw data ###################
#-----------------------------------------------------------------------------#

library(recipes)
ames_train_raw <- ames_orig %>%
  filter(`Sale Condition` == "Normal",
         # remove agricultural, commercial and industrial
         !(`MS Zoning` %in% c("A (agr)", "C (all)", "I (all)"))) %>%
  filter(PID %in% ames_train$pid) %>%
  select(-PID, -Order, -`Sale Condition`, -Utilities, -`Garage Finish`, -`Fireplace Qu`, -`Garage Cond`, -`Alley`, -`Total Bsmt SF`) %>%
  clean_names
  
ames_test_raw <- ames_orig %>%
  filter(`Sale Condition` == "Normal",
         # remove agricultural, commercial and industrial
         !(`MS Zoning` %in% c("A (agr)", "C (all)", "I (all)"))) %>%
  filter(PID %in% ames_test$pid) %>%
  select(-PID, -Order, -`Sale Condition`, -Utilities, -`Garage Finish`, -`Fireplace Qu`, -`Garage Cond`, -`Alley`, -`Total Bsmt SF`) %>%
  clean_names

model_recipe_raw <- recipe(sale_price ~ ., 
                       data = ames_train_raw)

model_recipe_steps_raw <- model_recipe_raw %>% 
  # convert the additional ingredients variable to dummy variables
  step_impute_mean(all_numeric()) %>%
  step_impute_mode(all_nominal()) %>%
  step_dummy(all_nominal()) %>%
  # remove predictor variables that are almost the same for every entry
  step_nzv(all_predictors()) 

prepped_recipe_raw <- prep(model_recipe_steps_raw, 
                       training = ames_train_raw)

ames_train_preprocessed_raw <- bake(prepped_recipe, ames_train_raw) 
ames_test_preprocessed_raw <- bake(prepped_recipe, ames_test_raw) 


# LS on the raw data
lm_raw <- lm(sale_price ~ ., data = ames_train_preprocessed_raw)
summary(lm_raw)$r.squared
test_pred_raw <- predict(lm_raw, ames_test_preprocessed)
cor(test_pred_raw[!is.na(test_pred_raw)], 
    ames_test_raw$sale_price[!is.na(test_pred_raw)])^2

# LS on the cleaned data
lm_clean <- lm(sale_price ~ ., data = select(ames, -pid))
summary(lm_clean)$r.squared
test_pred_clean <- predict(lm_clean, ames_test)
cor(test_pred_clean, 
    ames_test$sale_price)^2
