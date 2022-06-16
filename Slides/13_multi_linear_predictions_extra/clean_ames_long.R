# Preparing the Ames data for predictive modeling
library(tidyverse)
library(readxl)
library(fastDummies)
library(janitor)
ames_orig <- read_xls("AmesHousing.xls")
head(ames_orig) %>% print(width = Inf)


 
ames <- ames_orig %>%
  clean_names() %>%
  filter(sale_condition == "Normal",
         # remove agricultural, commercial and industrial
         !(`ms_zoning` %in% c("A (agr)", "C (all)", "I (all)"))) %>%
  transmute(sale_price = sale_price, 
         pid = pid,
         residential_density = case_when(ms_zoning == "RH" ~ 3,
                                         ms_zoning == "RM" ~ 2,
                                         ms_zoning == "RL" ~ 1,
                                         ms_zoning == "FV" ~ 0),
         irregular_lot_shape = case_when(lot_shape == "Reg" ~ 0,
                                         lot_shape == "IR1" ~ 1,
                                         lot_shape == "IR2" ~ 2,
                                         lot_shape == "IR3" ~ 3),
         quality = overall_qual,
         condition = overall_cond,
         functional = case_when(functional == "Typ" ~ 8,
                                functional == "Min1" ~ 7,
                                functional == "Min2" ~ 6,
                                functional == "Mod" ~ 5,
                                functional == "Maj1" ~ 4,
                                functional == "Maj2" ~ 3,
                                functional == "Sev" ~ 2,
                                functional == "Sal" ~ 1),
         year_built = year_built,
         #year_sold = Yr.Sold,
         year_remodeled = year_remod_add,
         gable_roof = if_else(roof_style == "Gable", 1, 0),
         masonry_veneer_brick = if_else(mas_vnr_type %in% c("BrkFace", "BrkCmn"), 1, 0),
         exterior_quality = case_when(exter_qual == "Ex" ~ 5,
                                      exter_qual == "Gd" ~ 4,
                                      exter_qual == "TA" ~ 3,
                                      exter_qual == "Fa" ~ 2,
                                      exter_qual == "Po" ~ 1),
         exterior_condition = case_when(exter_cond == "Ex" ~ 5,
                                        exter_cond == "Gd" ~ 4,
                                        exter_cond == "TA" ~ 3,
                                        exter_cond == "Fa" ~ 2,
                                        exter_cond == "Po" ~ 1),
         foundation_brick = if_else(foundation == "BrkTil", 1, 0),
         foundation_cinder = if_else(foundation == "CBlock", 1, 0),
         foundation_concrete = if_else(foundation == "PConc", 1, 0),
         basement_area = total_bsmt_sf,
         heating_quality = case_when(heating_qc == "Ex" ~ 5,
                                     heating_qc == "Gd" ~ 4,
                                     heating_qc == "TA" ~ 3,
                                     heating_qc == "Fa" ~ 2,
                                     heating_qc == "Po" ~ 1),
         first_floor_area = x1st_flr_sf,
         second_floor_area = x2nd_flr_sf,
         total_living_area = gr_liv_area,
         lot_area = lot_area,
         two_story = if_else(house_style %in% c("1Story", "SFoyer", "SLvl"), 0, 1),
         bedrooms = bedroom_abv_gr,
         electrical_standard = if_else(electrical == "SBrkr", 1, 0),
         kitchen_quality = case_when(kitchen_qual == "Ex" ~ 5,
                                     kitchen_qual == "Gd" ~ 4,
                                     kitchen_qual == "TA" ~ 3,
                                     kitchen_qual == "Fa" ~ 2,
                                     kitchen_qual == "Po" ~ 1),
         total_rooms = tot_rms_abv_grd,
         fireplaces = fireplaces,
         
         paved_drive = case_when(paved_drive == "Y" ~ 1,
                                 paved_drive == "P" ~ 0,
                                 paved_drive == "N" ~ -1),
         
         garage_attached = if_else(garage_type %in% c("Attchd", "BuiltIn", "2Types", "Basement"), 1, 0),
         garage_finish = case_when(garage_finish == "Fin" ~ 3,
                                   garage_finish == "RFn" ~ 2,
                                   garage_finish == "Unf" ~ 1,
                                   is.na(garage_finish) ~ 0,
                                   garage_finish == "NA" ~ 0,
                                   garage_finish == "" ~ 0),
         garage_cars = garage_cars,
         garage_area = garage_area,
         garage_year = if_else(!is.na(garage_yr_blt), garage_yr_blt, year_built),
         
         wood_deck_area = wood_deck_sf,
         
         fence_privacy = case_when(fence == "GdPrv" ~ 4,
                                   fence == "MnPrv" ~ 3,
                                   fence == "GdWo" ~ 2,
                                   fence == "MnWw" ~ 1,
                                   fence == "NA" ~ 0),
         sale_month = mo_sold,
         #sale_year = Yr.Sold,
         sale_price = sale_price,
         basement_quality = case_when(bsmt_qual == "Ex" ~ 5,
                                      bsmt_qual == "Gd" ~ 4,
                                      bsmt_qual == "TA" ~ 3,
                                      bsmt_qual == "Fa" ~ 2,
                                      bsmt_qual == "Po" ~ 1,
                                      bsmt_qual == "NA" ~ 3),
         basement_condition = case_when(bsmt_cond == "Ex" ~ 5,
                                        bsmt_cond == "Gd" ~ 4,
                                        bsmt_cond == "TA" ~ 3,
                                        bsmt_cond == "Fa" ~ 2,
                                        bsmt_cond == "Po" ~ 1,
                                        bsmt_cond == "NA" ~ 3),
         basement_exposure = case_when(bsmt_exposure == "Gd" ~ 4,
                                       bsmt_exposure == "Av" ~ 3,
                                       bsmt_exposure == "Mn" ~ 2,
                                       bsmt_exposure == "No" ~ 1,
                                       bsmt_exposure == "" ~ 1,
                                       bsmt_exposure == "NA" ~ 1,
                                       is.na(bsmt_exposure) ~ 1),
         basement_finished_rating = case_when(bsmt_fin_type_1 == "GLQ" ~ 5,
                                              bsmt_fin_type_1 == "ALQ" ~ 4,
                                              bsmt_fin_type_1 == "BLQ" ~ 3,
                                              bsmt_fin_type_1 == "Rec" ~ 4,
                                              bsmt_fin_type_1 == "LwQ" ~ 2,
                                              bsmt_fin_type_1 == "Unf" ~ 1,
                                              bsmt_fin_type_1 == "NA" ~ 1,
                                              bsmt_fin_type_1 == "" ~ 1),
         heating_quality = case_when(heating_qc == "Ex" ~ 5,
                                     heating_qc == "Gd" ~ 4,
                                     heating_qc == "TA" ~ 3,
                                     heating_qc == "Fa" ~ 2,
                                     heating_qc == "Po" ~ 1,
                                     heating_qc == "NA" ~ 5),
         garage_quality = case_when(garage_qual == "Ex" ~ 5,
                                    garage_qual == "Gd" ~ 4,
                                    garage_qual == "TA" ~ 3,
                                    garage_qual == "Fa" ~ 2,
                                    garage_qual == "Po" ~ 1,
                                    garage_qual == "NA" ~ 3),
         garage_condition = case_when(garage_cond == "Ex" ~ 5,
                                      garage_cond == "Gd" ~ 4,
                                      garage_cond == "TA" ~ 3,
                                      garage_cond == "Fa" ~ 2,
                                      garage_cond == "Po" ~ 1,
                                      garage_cond == "NA" ~ 3),
         fireplace_quality = case_when(fireplace_qu == "Ex" ~ 5,
                                       fireplace_qu == "Gd" ~ 4,
                                       fireplace_qu == "TA" ~ 3,
                                       fireplace_qu == "Fa" ~ 2,
                                       fireplace_qu == "Po" ~ 1,
                                       fireplace_qu == "NA" ~ 2),
         bathrooms = full_bath + bsmt_full_bath + 0.5 * (half_bath + bsmt_half_bath),
         porch = if_else(open_porch_sf != 0 |
                          enclosed_porch != 0 |
                           x3ssn_porch != 0 |
                           screen_porch != 0, 1, 0),
         porch_area = open_porch_sf + enclosed_porch + x3ssn_porch + screen_porch,
         exterior_vinyl = if_else(exterior_1st == "VinylSd" | exterior_2nd == "VinylSd", 1, 0),
         exterior_metal = if_else(exterior_1st == "MetalSd" | exterior_2nd == "MetalSd", 1, 0),
         exterior_hardboard = if_else(exterior_1st == "HdBoard" | exterior_2nd == "HdBoard", 1, 0),
         
         exterior_wood = if_else(exterior_1st %in% c("Wd Sdng", "Wd Shng", "WdShing", "Plywood") | exterior_2nd %in% c("Wd Sdng", "Wd Shng", "WdShing", "Plywood"), 1, 0),
         lot_inside = if_else(lot_config == "Inside", 1, 0),
         lot_corner = if_else(lot_config == "Corner", 1, 0)) 
  
  
  
  
set.seed(782264)
# create 70% training set
ames_train <- ames %>% sample_frac(0.7)
# create 70% test set
ames_test <- ames %>% filter(!(pid %in% ames_train$pid))
ames_train <- ames_train %>% select(-pid)
ames_test <- ames_test %>% select(-pid)
# write data
write_csv(ames_train, "ames_train_full.csv")
write_csv(ames_test, "ames_test_full.csv")


# Note: if you want to be really careful to keep your training and testing sets 
# independent, you should generate your cleaning procedure using the 
# training data only, and write a function that you can then apply to your test data



lm_fit <- lm(sale_price ~ ., ames_train)
pred <- predict(lm_fit, ames_test)
cor(pred, ames_test$sale_price)
