library(dplyr)
library(stringr)

fix_levels <- function(categorical) {
  categorical %>% str_replace_all(" +", "_") %>% ifelse(. == "", "none", .)
}

load_application <- function(filename) {
  read.csv(filename) %>%
    setNames(names(.) %>% tolower()) %>%
    mutate(
      name_type_suite = fix_levels(name_type_suite),
      fondkapremont_mode = fix_levels(fondkapremont_mode)
    )
}

data_train <- load_application("data/application_train.csv")
data_test  <- load_application("data/application_test.csv")

data_train$target = factor(data_train$target, labels = c("no", "yes"))

# FILL MISSING --------------------------------------------------------------------------------------------------------

data_train <- data_train %>% mutate(
  amt_annuity = ifelse(is.na(amt_annuity), median(data_train$amt_annuity, na.rm = TRUE), amt_annuity),
  amt_goods_price = ifelse(is.na(amt_goods_price), median(data_train$amt_goods_price, na.rm = TRUE), amt_goods_price)
)
#
# Use values from *training* data for imputation?
#
data_test <- data_test %>% mutate(
  amt_annuity = ifelse(is.na(amt_annuity), median(data_train$amt_annuity, na.rm = TRUE), amt_annuity)
)

# ENGINEER ------------------------------------------------------------------------------------------------------------

data_train <- data_train %>% mutate(
  own_car = !is.na(own_car_age)
)
#
# Use values from *training* data for imputation?
#
data_test <- data_test %>% mutate(
  own_car = !is.na(own_car_age)
)

# REBALANCE -----------------------------------------------------------------------------------------------------------

