library(tidyverse)
library(yardstick)

ames_train <- read_csv("ames_train_full.csv") 
ames_test <- read_csv("ames_test_full.csv") 
ames_train %>% print(width = Inf)
dim(ames_train)

# remove order and pid variables


# get performance on full set of features
lm_full <- lm(sale_price ~ ., ames_train)
pred_test <- predict(lm_full, ames_test)
rmse <- rmse_vec(ames_test$sale_price, pred_test)
rmse
rsq <- rsq_vec(ames_test$sale_price, pred_test)
rsq





#-----------------------------------------------------------------------------#
######################## Explore collinearity #################################
#-----------------------------------------------------------------------------#

# identify highly correlated variables
cor_df <- ames_train %>% 
  # get correlation matrix
  cor() %>%
  # convert to data frame
  as.data.frame() %>%
  # create column from rownames
  rownames_to_column(var = "var1") %>%
  # create long-form data
  pivot_longer(2:(ncol(ames_train) + 1), 
               names_to = "var2", values_to = "cor")

cor_df %>%
  # remove rows computing correlation with sale price or of a variable with itself
  filter(var1 != "sale_price", var2 != "sale_price",
         var1 != var2) %>%
  # arrange in decreasing absolute value correlation
  arrange(desc(abs(cor)))

# identify which of each pair is more correlated with sale price
cor_df %>% 
  filter(var1 == "sale_price", var2 %in% c("garage_area", "garage_cars"))
cor_df %>% 
  filter(var1 == "sale_price", var2 %in% c("second_floor_area", "two_story"))
cor_df %>% 
  filter(var1 == "sale_price", var2 %in%  c("year_built", "garage_year"))
cor_df %>% 
  filter(var1 == "sale_price", var2 %in%  c("total_living_area", "total_rooms"))

# remove highly correlated variables
ames_train2 <- ames_train %>%
  select(-garage_area, 
         -garage_year, 
         -total_rooms, 
         -two_story)

# fit LS
lm_cor2 <- lm(sale_price ~ ., data = ames_train2)
# generate test set predictions
pred_test_cor2 <- predict(lm_cor2, ames_test)
# compute rMSE
rmse <- rmse_vec(ames_test$sale_price, pred_test_cor2)
rmse
# compute Rsquared
rsq <- rsq_vec(ames_test$sale_price, pred_test_cor2)
rsq
# the performance actually got worse, but only very slightly






#----------------------------------------------------------------------------#
########################## Correlation screening selection #####################
#----------------------------------------------------------------------------#

# compute the correlation of each feature with sale price
cor_df <- ames_train %>%
  select_if(is.numeric) %>%
  # compute correlation matrix
  cor() %>%
  as.data.frame() %>%
  # pull sale_price column
  select(sale_price_cor = sale_price) %>%
  # arrange in decreasing order of (abs val) sale price correlation
  arrange(desc(abs(sale_price_cor))) %>%
  rownames_to_column(var = "variable")
head(cor_df)

# generate a vector of the variables whose correlation is at least 0.5
selected_vars <- cor_df %>%
  filter(sale_price_cor > 0.3) %>%
  pull(variable)
selected_vars
length(selected_vars)

# keep only correlated variables
ames_train_cor <- ames_train %>%
  select(one_of(selected_vars))

# fit LS
lm_cor <- lm(sale_price ~ ., data = ames_train_cor)
# generate test set predictions
pred_test_cor <- predict(lm_cor, ames_test)
# compute rMSE
rmse <- rmse_vec(ames_test$sale_price, pred_test_cor)
rmse
# compute Rsquared
rsq <- rsq_vec(ames_test$sale_price, pred_test_cor)
rsq
