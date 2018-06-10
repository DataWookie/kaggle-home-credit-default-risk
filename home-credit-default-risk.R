# There are 7 different sources of data:
# •application_train/application_test: the main training and testing data with information about each loan application at Home Credit. Every loan has its own row and is identified by the featureSK_ID_CURR. The training application data comes with theTARGETindicating 0: the loan was repaid or 1: the loan was not repaid.
# •bureau: data concerning client's previous credits from other financial institutions. Each previous credit has its own row in bureau, but one loan in the application data can have multiple previous credits.
# •bureau_balance: monthly data about the previous credits in bureau. Each row is one month of a previous credit, and a single previous credit can have multiple rows, one for each month of the credit length.
# •previous_application: previous applications for loans at Home Credit of clients who have loans in the application data. Each current loan in the application data can have multiple previous loans. Each previous application has one row and is identified by the featureSK_ID_PREV.
# •POS_CASH_BALANCE: monthly data about previous point of sale or cash loans clients have had with Home Credit. Each row is one month of a previous point of sale orcash loan, and a single previous loan can have many rows.
# •credit_card_balance: monthly data about previous credit cards clients have had with Home Credit. Each row is one month of a credit card balance, and a single credit card can have many rows.
# •installments_payment: payment history for previous loans at Home Credit. There is one row for every made payment and one row for every missed payment.

# =====================================================================================================================
# = Kaggle: Home Credit Default Risk                                                                                  =
# =                                                                                                                   =
# = Author: Andrew B. Collier <andrew@exegetic.biz> | @datawookie                                                     =
# =====================================================================================================================

# TODO: TRY AGGREGATING days_employed

# TODO: LOOK AT amt_req_credit_bureau_hour
# TODO: LOOK AT amt_req_credit_bureau_day
# TODO: LOOK AT amt_req_credit_bureau_week

# TODO: CONVERT NA TO CATEGORY FOR CATEGORICAL VARIABLES.





# CONFIGURATION -------------------------------------------------------------------------------------------------------

set.seed(13)
#
DEBUG = as.logical(Sys.getenv("DEBUG", TRUE))

# LIBRARIES -----------------------------------------------------------------------------------------------------------

library(dplyr)
library(stringr)
library(forcats)
library(caret)

fix_levels <- function(categorical) {
  categorical %>% str_replace_all("[:/]", "") %>% str_replace_all(" +", "_") %>% ifelse(. == "", "none", .) %>% tolower() %>% factor()
}

load_application <- function(filename) {
  read.csv(filename) %>%
    setNames(names(.) %>% tolower()) %>%
    mutate(
      name_type_suite = fix_levels(name_type_suite),
      name_income_type = fix_levels(name_income_type),
      name_family_status = fix_levels(name_family_status),
      fondkapremont_mode = fix_levels(fondkapremont_mode),
      organization_type = fix_levels(organization_type)
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

missing_to_zero <- function(values) {
  ifelse(is.na(values), 0, values)
}

# TODO: These might better be replaced with mean/median.
#
data <- data %>% mutate(
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
  ),
  organization_type = organization_type %>% fct_collapse(
    industry_type_other = c("industry_type_8", "industry_type_13")
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
  "flag_emp_phone",
  "flag_mobil",
  "flag_document_2",
  "flag_document_4",
  "flag_document_10",
  "flag_document_12",
  "flag_document_17",
  "flag_document_21",
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

# IDENTIFY COLUMNS TO REMOVE ------------------------------------------------------------------------------------------

# nearZeroVar(data_train)
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

# SPLIT ---------------------------------------------------------------------------------------------------------------

# Split according to train/test and remove index column.
#
data <- split(data, data$set) %>% lapply(function(df) df %>% select(-set))
#
data_test <- data$test
data_train <- data$train %>% select(-sk_id_curr)
#
rm(data)

# DOWNSAMPLE ----------------------------------------------------------------------------------------------------------

if (DEBUG) {
  data_train <- rbind(
    data_train %>% filter(target == "yes") %>% sample_n(15000),
    data_train %>% filter(target == "no") %>% sample_n(15000)
  )
}

# MODEL METHOD --------------------------------------------------------------------------------------------------------

# Attempted methods:
#
# - glm
# - rpart
# - svmRadial
# - xgbTree
#
METHOD = Sys.getenv("METHOD", "glm")

# CREATE MATRICES -----------------------------------------------------------------------------------------------------

X_train = data_train %>% select(-target)
y_train = data_train %>% pull(target)
#
X_test  = data_test %>% select(-target, -sk_id_curr)

# Convert factors to dummy variables.
#
if (METHOD %in% c("xgbTree", "svmRadial")) {
  X_train = predict(dummyVars(~ ., data = X_train), X_train)
  X_test  = predict(dummyVars(~ ., data = X_test), X_test)
}

# TRAIN ---------------------------------------------------------------------------------------------------------------

fit <- train(x = X_train,
             y = y_train,
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

# fit <- train(x = X_train,
#              y = y_train,
#              method = "svmRadial",
#              preProcess = "medianImpute",
#              metric = "ROC",
#              trControl = trainControl(
#                method = "cv",
#                number = 10,
#                classProbs = TRUE,
#                summaryFunction = twoClassSummary,
#                verboseIter = TRUE
#              ))

# SUBMISSION ----------------------------------------------------------------------------------------------------------

TIME <- format(Sys.time(), "%Y%m%d-%H%M%S")

sink(sprintf("%s.txt", TIME))
print(fit)
cat("\n")
fit$results %>% arrange(desc(ROC))
sink()

saveRDS(fit, file = sprintf("%s.rds", TIME))

submission = cbind(SK_ID_CURR = data_test$sk_id_curr, TARGET = predict(fit, X_test, type = "prob")[,2]) %>%
  data.frame() %>%
  #
  # This is necessary to ensure that output not converted to scientific notation.
  #
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR))

write.csv(submission, sprintf("%s.csv", TIME), quote = FALSE, row.names = FALSE)
