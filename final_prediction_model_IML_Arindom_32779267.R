library(tidymodels)
library(rsample)
library(recipes)
library(parsnip)
library(tune)
library(dials)
library(workflows)
library(yardstick)
library(tidyverse)

# Name: Arindom Baruah
# Student ID: 32779267
# E-mail ID: abar0090@student.monash.edu
# Kaggle user name: arindombaruah


set.seed(1148)
water <- read_csv("data/water_train.csv") |> 
  mutate(status_id = factor(status_id)) 
water_test <- read_csv("data/water_test.csv") 


# Parallel processing enabled for tuning


############## !!!!!!! Takes very long time to run (3-5 hours)  !!!!!!!!! ######################

############## Tuned results are specified below to save time ################

# Random Forest Model Specification (classification)
# 
# Main Arguments after tuning :
#   mtry = 5
#   trees = 1000
#   min_n = 2


# 
# # Setting tune workflow
# 
# tune_spec <- rand_forest(
#   mtry = tune(),
#   trees = 1000,
#   min_n = tune()
# ) %>%
#   set_mode("classification") %>%
#   set_engine("ranger")
# 
# 
# 
# 
# # Tune workflow
# 
# tune_wf <- workflow() %>%
#   add_model(tune_spec) %>%
#   add_formula(as.factor(status_id)~.)
# 
# # Setting range of hyperparameters for tuning
# 
# rf_grid <- grid_regular(
#   mtry(range = c(5, 30)),
#   min_n(range = c(2, 8)),
#   levels = 5
# )
# 
# rf_grid
# 
# set.seed(1148)
# 
# rf_folds <- vfold_cv(water[,-1],v = 5) # Use the selection of variables in water dataset
# 

# doParallel::registerDoParallel()
# set.seed(1148)
# 
# rf_res <- tune_grid(
#   tune_wf,
#   resamples = rf_folds,
#   grid = rf_grid
# )
# rf_res
# 
# 
# 
# rf_res %>%
#   collect_metrics() %>% head() %>% kbl()
# 
# 
# 
# 
# best_acc <- select_best(rf_res, "accuracy")
# 
# 
# 
# final_rf <- finalize_model(
#   tune_spec,
#   best_acc
# )
# 
# final_rf


# ################################ End of tuning ##################################################

# Can directly set the below tuned specs 


tune_spec <- rand_forest(
  mtry = 5,
  trees = 1000,
  min_n = 2
) %>%
  set_mode("classification") %>%
  set_engine("ranger")


# Utilising only year of report date to reduce splits

water$report_date <- year(water$report_date)
water_test$report_date <- year(water_test$report_date)

# Choosing the final set of variables

water <- water %>% select(-is_urban)
water_test <- water_test %>% select(-is_urban)


# Choosing report date as just year and not date

water$report_date <- year(water$report_date)
water_test$report_date <- year(water_test$report_date)


# Removing junk data

water <- water %>% filter(install_year < 2024)


set.seed(1148)
rf_fit_tune <- tune_spec %>%
  fit(status_id~.,data = water[,-1]) # Use the selection of variables in water dataset


# Changing prediction thresholds for "no" as >=0.45

water_ts_pred <- water_test |>
  bind_cols(predict(rf_fit_tune, new_data=water_test, type="prob")) |>
  mutate(pstatus_id = if_else(.pred_n >=0.45,"n","y"))

write_csv(water_ts_pred[,c("ID", "pstatus_id")], file="final_predictions.csv")



