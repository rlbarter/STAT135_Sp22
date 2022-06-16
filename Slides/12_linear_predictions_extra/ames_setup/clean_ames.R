# clean the Ames data
library(tidyverse)
library(readxl)
ames_tmp <- readxl::read_xls("AmesHousing.xls")
ames_simplified <- ames_tmp %>% 
  filter(`Sale Condition` == "Normal",
         # remove agricultural, commercial and industrial
         !(`MS Zoning` %in% c("A (agr)", "C (all)", "I (all)"))) %>%
  select(sale_price = SalePrice, total_living_area = `Gr Liv Area`) %>%
  mutate(id = 1:n())

ames_train <- ames_simplified %>% 
  sample_frac(0.7)
ames_test <- ames_simplified %>%
  filter(!(id %in% ames_train$id)) %>%
  select(-id)
ames_train <- ames_train %>% select(-id)
write_csv(ames_train, "../ames_train.csv")
write_csv(ames_test, "../ames_test.csv")
