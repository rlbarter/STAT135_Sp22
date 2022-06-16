# Deciding how to clean the Ames data for predictive modeling
library(tidyverse)
library(readxl)
library(fastDummies)
ames_orig <- read_xls("AmesHousing.xls")
head(ames_orig) %>% print(width = Inf)


#-----------------------------------------------------------------------------#
######################## Clean variable names #################################
#-----------------------------------------------------------------------------#

library(janitor)
ames <- clean_names(ames_orig)
head(ames) %>% print(width = Inf)


#-----------------------------------------------------------------------------#
######################## explore missing values ###############################
#-----------------------------------------------------------------------------#

# count the number of missing values in each column
ames %>%
  map_dbl(~sum(is.na(.))) %>%
  sort




# We probably want to replace each NA categorical variable with the mode
ames %>% count(bsmt_fin_type_2)
ames %>% count(electrical)

# What about numeric variables? Replace with the mean?
ames %>% count(garage_yr_blt) %>% 
  as.data.frame
# replace with mean, or replace with year the house was built?
# ames <- ames %>%
#   mutate(garage_yr_blt = if_else(is.na(garage_yr_blt), 
#                                  year_built, 
#                                  garage_yr_blt))


#-----------------------------------------------------------------------------#
#################### Handling categorical variables ###########################
#-----------------------------------------------------------------------------#


# consider the bsmt_qual variable
ames %>% count(bsmt_qual)
# how is a computer going to interpret these levels? ..... alphabetically? 
# Should we create dummy variables? This will remove order information
# Should we convert to numeric values?
# What should we do with the "NA" (no basement)?

# can we simplify variables with many small levels?
ames %>% count(roof_style)

# are all of these categorical variables actually necessary?
ames %>% count(condition_2)


#-----------------------------------------------------------------------------#
#################### Feature engineering ###########################
#-----------------------------------------------------------------------------#

# there isn't a "bathrooms" variable
ames %>% 
  transmute(total_sq_feet = total_bsmt_sf + x1st_flr_sf + x2nd_flr_sf,
            bathrooms = full_bath + bsmt_full_bath + 0.5 * (half_bath + bsmt_half_bath))
         

# using explorations like these, we can clean the data - see clean_ames_long.R
         


