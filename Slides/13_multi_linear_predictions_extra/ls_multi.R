library(tidyverse)
library(fastDummies)
library(scales)

ames_train <- read_csv("ames_train_multi.csv")
ames_test <- read_csv("ames_test_multi.csv")
ames_train %>% print(width = Inf)


# create dummy variables (from fastDummies package)
ames_train_dummy <- dummy_cols(ames_train, 
                               select_columns = "neighborhood",
                               remove_selected_columns = TRUE, 
                               remove_first_dummy = TRUE) 
head(ames_train_dummy) %>% print(width = Inf)
colnames(ames_train_dummy) 

# the reference level above was CollgCr
# view all neighborhood levels
ames_train %>% count(neighborhood)



# Let's force the reference column to be "Other"
# set the first level of the neighborhood factor to be "Other"
ames_train <- ames_train %>% 
  mutate(neighborhood = fct_relevel(neighborhood, "Other"))
# then re-create the dataset with the dummy variable
ames_train_dummy <- dummy_cols(ames_train, 
                               select_columns = "neighborhood",
                               remove_selected_columns = TRUE, 
                               remove_first_dummy = TRUE) 
colnames(ames_train_dummy)



# set up for the matrix computation
X <- ames_train_dummy %>%
  # add intercept column
  mutate(intercept = 1) %>%
  # remove response variable
  select(-sale_price) %>%
  select(intercept, everything()) %>%
  as.matrix
head(X)
y <- ames_train_dummy$sale_price

# solve for beta manually:
# solve() inverts a matrix
# (X^TX)^{-1}X^TY
beta <- solve(t(X) %*% X) %*% t(X) %*% y
beta
# generate training set predictions
pred_train_manual <- X %*% beta
head(pred_train_manual)
# compute training R-squared
cor(pred_train_manual, y)^2





# fitting the LS algorithm with lm()
lm_multi <- lm(sale_price ~ ., data = ames_train_dummy)
# lm_multi <- lm(sale_price ~ total_living_area + quality_score +
#                  year_built + bedrooms, data = ames_train_dummy)
lm_multi
# generate predictions
pred_train <- predict(lm_multi, ames_train_dummy)
# compute training R-squared
cor(pred_train, ames_train_dummy$sale_price)^2
# confirm with lm_multi version
summary(lm_multi)$r.squared



#-----------------------------------------------------------------------------#
#################### Evaluating on the test set ###############################
#-----------------------------------------------------------------------------#

# For the manual version, we need to create dummy variables for the test data
ames_test <- ames_test %>% 
  mutate(neighborhood = fct_relevel(neighborhood, "Other"))
ames_test_dummy <- dummy_cols(ames_test, 
                               select_columns = "neighborhood",
                               remove_selected_columns = TRUE, 
                               remove_first_dummy = TRUE) %>%
  mutate_all(~replace_na(., 0))
X_test <- ames_test_dummy %>%
  mutate(intercept = 1) %>%
  select(-sale_price) %>%
  select(intercept, everything()) %>%
  as.matrix
y_test <- ames_test_dummy$sale_price


# generate test set predictions
pred_test_manual <- X_test %*% beta
# look at the first 6 predictions
head(pred_test_manual)
# compute test set R-squared
rsq_test <- cor(pred_test_manual, y_test)^2
rsq_test





# generating predictions using lm_multi
test_pred <- predict(lm_multi, ames_test_dummy)
head(test_pred)
# compute test set R-squared
rsq_test <- cor(test_pred, ames_test$sale_price)^2
rsq_test
