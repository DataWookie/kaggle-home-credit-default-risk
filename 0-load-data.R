library(dplyr)
library(stringr)
library(caret)

# TODO: CONVERT NA TO CATEGORY FOR CATEGORICAL VARIABLES.
#
#
fix_levels <- function(categorical) {
  categorical %>% str_replace_all(" +", "_") %>% ifelse(. == "", "none", .) %>% factor()
}

load_application <- function(filename) {
  read.csv(filename) %>%
    setNames(names(.) %>% tolower()) %>%
    mutate(
      name_type_suite = fix_levels(name_type_suite),
      fondkapremont_mode = fix_levels(fondkapremont_mode)
    ) %>%
    mutate(
      flag_not_employed = ifelse(days_employed == 365243, 'Y', 'N') %>% factor(),
      days_employed = ifelse(days_employed == 365243, 0, days_employed)
    )
}

data_train <- load_application("data/application_train.csv") %>%
  mutate(set = "train")
data_test  <- load_application("data/application_test.csv") %>%
  mutate(set = "test", target = NA) %>%
  select(sk_id_curr, target, everything())

data_train$target = factor(data_train$target, labels = c("no", "yes"))

data <- rbind(data_train, data_test)

# OUTLIERS ------------------------------------------------------------------------------------------------------------

data <- data %>% mutate(
  obs_30_cnt_social_circle = ifelse(obs_30_cnt_social_circle > 100, NA, obs_30_cnt_social_circle),
  obs_60_cnt_social_circle = ifelse(obs_30_cnt_social_circle > 100, NA, obs_60_cnt_social_circle)
)

# FILL MISSING --------------------------------------------------------------------------------------------------------

# Impute median value using only values for training data.
#
impute_median <- function(col) {
  median_train <- data %>% filter(set == "train") %>% pull(col) %>% median(na.rm = TRUE)
  #
  ifelse(is.na(data[, col]), median_train, data[, col])
}

missing_to_zero <- function(values) {
  ifelse(is.na(values), 0, values)
}

data <- data %>% mutate(
  amt_annuity = impute_median("amt_annuity"),
  amt_goods_price = impute_median("amt_goods_price"),
  cnt_fam_members = impute_median("cnt_fam_members"),
  commonarea_medi = impute_median("commonarea_medi"),
  commonarea_mode = impute_median("commonarea_mode"),
  elevators_medi = impute_median("elevators_medi"),
  elevators_mode = impute_median("elevators_mode"),
  entrances_mode = impute_median("entrances_mode"),
  entrances_medi = impute_median("entrances_medi"),
  floorsmax_mode = impute_median("floorsmax_mode"),
  floorsmax_medi = impute_median("floorsmax_medi"),
  floorsmin_mode = impute_median("floorsmin_mode"),
  floorsmin_medi = impute_median("floorsmin_medi"),
  landarea_mode = impute_median("landarea_mode"),
  landarea_medi = impute_median("landarea_medi"),
  livingarea_mode = impute_median("livingarea_mode"),
  livingarea_medi = impute_median("livingarea_medi"),
  nonlivingarea_mode = impute_median("nonlivingarea_mode"),
  nonlivingarea_medi = impute_median("nonlivingarea_medi"),
  livingapartments_mode = impute_median("livingapartments_mode"),
  livingapartments_medi = impute_median("livingapartments_medi"),
  nonlivingapartments_mode = impute_median("nonlivingapartments_mode"),
  nonlivingapartments_medi = impute_median("nonlivingapartments_medi"),
  obs_30_cnt_social_circle = impute_median("obs_30_cnt_social_circle"),
  def_30_cnt_social_circle = impute_median("def_30_cnt_social_circle"),
  obs_60_cnt_social_circle = impute_median("obs_60_cnt_social_circle"),
  def_60_cnt_social_circle = impute_median("def_60_cnt_social_circle"),
  amt_req_credit_bureau_hour = impute_median("amt_req_credit_bureau_hour"),
  amt_req_credit_bureau_day = impute_median("amt_req_credit_bureau_day"),
  amt_req_credit_bureau_week = impute_median("amt_req_credit_bureau_week"),
  amt_req_credit_bureau_mon = impute_median("amt_req_credit_bureau_mon"),
  amt_req_credit_bureau_qrt = impute_median("amt_req_credit_bureau_qrt"),
  amt_req_credit_bureau_year = impute_median("amt_req_credit_bureau_year"),
  apartments_mode = impute_median("apartments_mode"),
  apartments_medi = impute_median("apartments_medi"),
  basementarea_mode = impute_median("basementarea_mode"),
  basementarea_medi = impute_median("basementarea_medi"),
  totalarea_mode = impute_median("totalarea_mode"),
  years_beginexpluatation_mode = impute_median("years_beginexpluatation_mode"),
  years_beginexpluatation_medi = impute_median("years_beginexpluatation_medi"),
  years_build_mode = impute_median("years_build_mode"),
  years_build_medi = impute_median("years_build_medi"),
  days_last_phone_change = impute_median("days_last_phone_change"),
  #
  # TODO: These might better be replaced with mean/median.
  #
  own_car_age = missing_to_zero(own_car_age),
  ext_source_1 = missing_to_zero(ext_source_1),
  ext_source_2 = missing_to_zero(ext_source_2),
  ext_source_3 = missing_to_zero(ext_source_3)
)

# ENGINEER ------------------------------------------------------------------------------------------------------------

data <- data %>%
  mutate(
    days_employed_percent = days_employed / days_birth,
    income_credit_percent = amt_income_total /amt_credit,
    income_per_person = amt_income_total / cnt_fam_members,
    annuity_income_percent = amt_annuity / amt_income_total
  )

# REBALANCE -----------------------------------------------------------------------------------------------------------

# SPLIT ---------------------------------------------------------------------------------------------------------------

data <- split(data, data$set)
#
data_test <- data$test
data_train <- data$train
#
rm(data)