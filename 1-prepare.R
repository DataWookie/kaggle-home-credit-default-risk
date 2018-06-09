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

train <- data_train %>% select(-one_of(EXCLUDE))
test <- data_test %>% select(-one_of(EXCLUDE))

# IDENTIFY COLUMNS TO REMOVE ------------------------------------------------------------------------------------------

# nearZeroVar(train)
#
train_numeric = train[, sapply(train, class) != "factor"]
#
index_correlated = findCorrelation(cor(train_numeric), cutoff = 0.975)
index_linear = findLinearCombos(train_numeric)
#
names(train_numeric)[index_correlated]
index_linear
#
rm(train_numeric)

# DOWNSAMPLE ----------------------------------------------------------------------------------------------------------

train <- rbind(
  train %>% filter(target == "yes") %>% sample_n(15000),
  train %>% filter(target == "no") %>% sample_n(15000)
)
