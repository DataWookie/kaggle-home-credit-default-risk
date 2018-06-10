# There are 7 different sources of data:
# •application_train/application_test: the main training and testing data with information about each loan application at Home Credit. Every loan has its own row and is identified by the featureSK_ID_CURR. The training application data comes with theTARGETindicating 0: the loan was repaid or 1: the loan was not repaid.
# •bureau: data concerning client's previous credits from other financial institutions. Each previous credit has its own row in bureau, but one loan in the application data can have multiple previous credits.
# •bureau_balance: monthly data about the previous credits in bureau. Each row is one month of a previous credit, and a single previous credit can have multiple rows, one for each month of the credit length.
# •previous_application: previous applications for loans at Home Credit of clients who have loans in the application data. Each current loan in the application data can have multiple previous loans. Each previous application has one row and is identified by the featureSK_ID_PREV.
# •POS_CASH_BALANCE: monthly data about previous point of sale or cash loans clients have had with Home Credit. Each row is one month of a previous point of sale orcash loan, and a single previous loan can have many rows.
# •credit_card_balance: monthly data about previous credit cards clients have had with Home Credit. Each row is one month of a credit card balance, and a single credit card can have many rows.
# •installments_payment: payment history for previous loans at Home Credit. There is one row for every made payment and one row for every missed payment.

library(dplyr)
library(stringr)
library(forcats)
library(caret)

# TODO: CONVERT NA TO CATEGORY FOR CATEGORICAL VARIABLES.
#
#
fix_levels <- function(categorical) {
  categorical %>% str_replace_all("/", "") %>% str_replace_all(" +", "_") %>% ifelse(. == "", "none", .) %>% tolower() %>% factor()
}

load_application <- function(filename) {
  read.csv(filename) %>%
    setNames(names(.) %>% tolower()) %>%
    mutate(
      name_type_suite = fix_levels(name_type_suite),
      name_income_type = fix_levels(name_income_type),
      name_family_status = fix_levels(name_family_status),
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
  # amt_annuity = impute_median("amt_annuity"),
  # amt_goods_price = impute_median("amt_goods_price"),
  # cnt_fam_members = impute_median("cnt_fam_members"),
  # commonarea_medi = impute_median("commonarea_medi"),
  # commonarea_mode = impute_median("commonarea_mode"),
  # elevators_medi = impute_median("elevators_medi"),
  # elevators_mode = impute_median("elevators_mode"),
  # entrances_mode = impute_median("entrances_mode"),
  # entrances_medi = impute_median("entrances_medi"),
  # floorsmax_mode = impute_median("floorsmax_mode"),
  # floorsmax_medi = impute_median("floorsmax_medi"),
  # floorsmin_mode = impute_median("floorsmin_mode"),
  # floorsmin_medi = impute_median("floorsmin_medi"),
  # landarea_mode = impute_median("landarea_mode"),
  # landarea_medi = impute_median("landarea_medi"),
  # livingarea_mode = impute_median("livingarea_mode"),
  # livingarea_medi = impute_median("livingarea_medi"),
  # nonlivingarea_mode = impute_median("nonlivingarea_mode"),
  # nonlivingarea_medi = impute_median("nonlivingarea_medi"),
  # livingapartments_mode = impute_median("livingapartments_mode"),
  # livingapartments_medi = impute_median("livingapartments_medi"),
  # nonlivingapartments_mode = impute_median("nonlivingapartments_mode"),
  # nonlivingapartments_medi = impute_median("nonlivingapartments_medi"),
  # obs_30_cnt_social_circle = impute_median("obs_30_cnt_social_circle"),
  # def_30_cnt_social_circle = impute_median("def_30_cnt_social_circle"),
  # obs_60_cnt_social_circle = impute_median("obs_60_cnt_social_circle"),
  # def_60_cnt_social_circle = impute_median("def_60_cnt_social_circle"),
  # amt_req_credit_bureau_hour = impute_median("amt_req_credit_bureau_hour"),
  # amt_req_credit_bureau_day = impute_median("amt_req_credit_bureau_day"),
  # amt_req_credit_bureau_week = impute_median("amt_req_credit_bureau_week"),
  # amt_req_credit_bureau_mon = impute_median("amt_req_credit_bureau_mon"),
  # amt_req_credit_bureau_qrt = impute_median("amt_req_credit_bureau_qrt"),
  # amt_req_credit_bureau_year = impute_median("amt_req_credit_bureau_year"),
  # apartments_mode = impute_median("apartments_mode"),
  # apartments_medi = impute_median("apartments_medi"),
  # basementarea_mode = impute_median("basementarea_mode"),
  # basementarea_medi = impute_median("basementarea_medi"),
  # totalarea_mode = impute_median("totalarea_mode"),
  # years_beginexpluatation_mode = impute_median("years_beginexpluatation_mode"),
  # years_beginexpluatation_medi = impute_median("years_beginexpluatation_medi"),
  # years_build_mode = impute_median("years_build_mode"),
  # years_build_medi = impute_median("years_build_medi"),
  # days_last_phone_change = impute_median("days_last_phone_change"),
  #
  # TODO: These might better be replaced with mean/median.
  #
  own_car_age = missing_to_zero(own_car_age),
  ext_source_1 = missing_to_zero(ext_source_1),
  ext_source_2 = missing_to_zero(ext_source_2),
  ext_source_3 = missing_to_zero(ext_source_3)
)

# FACTORS -------------------------------------------------------------------------------------------------------------

data <- data %>% mutate(
  # The rare levels in the original factor are a problem with cross-validation.
  name_income_type = name_income_type %>% fct_collapse(
    working = c("working", "businessman"),
    unemployed = c("unemployed", "student", "maternity_leave")
    ),
  name_family_status = name_family_status %>% fct_collapse(
    married = c("married", "unknown")
  )
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

# SELECT FEATURES -----------------------------------------------------------------------------------------------------

EXCLUDE <- c(
  "sk_id_curr",
  "flag_emp_phone",
  "amt_credit",
  "region_rating_client_w_city",
  "elevators_avg",
  "commonarea_avg",
  "entrances_avg",
  "floorsmax_avg",
  "floorsmin_avg",
  "landarea_avg",
  "livingarea_avg",
  "nonlivingarea_avg",
  "livingapartments_avg",
  "nonlivingapartments_avg",
  "basementarea_avg",
  "apartments_avg",
  "years_beginexpluatation_avg",
  "years_build_avg"
)

data <- data %>% select(-one_of(EXCLUDE))

# SPLIT ---------------------------------------------------------------------------------------------------------------

# Split according to train/test and remove index column.
#
data <- split(data, data$set) %>% lapply(function(df) df %>% select(-set))
#
data_test <- data$test
data_train <- data$train
#
rm(data)

# IDENTIFY COLUMNS TO REMOVE ------------------------------------------------------------------------------------------

# nearZeroVar(train)
#
# train_numeric = train[, sapply(train, class) != "factor"]
# #
# index_correlated = findCorrelation(cor(train_numeric), cutoff = 0.975)
# index_linear = findLinearCombos(train_numeric)
# #
# names(train_numeric)[index_correlated]
# index_linear
# #
# rm(train_numeric)

# DOWNSAMPLE ----------------------------------------------------------------------------------------------------------

data_train <- rbind(
  data_train %>% filter(target == "yes") %>% sample_n(15000),
  data_train %>% filter(target == "no") %>% sample_n(15000)
)

# TRAIN ---------------------------------------------------------------------------------------------------------------

# Attempted methods:
#
# - glm
# - rpart
# - svmRadial
# - xgbTree
#
METHOD = "glm"

fit <- train(x = data_train %>% select(-target),
             y = data_train %>% pull(target),
             method = METHOD,
             preProcess = "medianImpute",
             metric = "ROC",
             trControl = trainControl(
               method = "cv",
               number = 10,
               classProbs = TRUE,
               summaryFunction = twoClassSummary,
               verboseIter = TRUE
             ))
