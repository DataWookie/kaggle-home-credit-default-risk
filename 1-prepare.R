COLUMNS = c(
  "amt_annuity",
  "amt_goods_price",
  "own_car_age",
  "cnt_fam_members",
  "ext_source_1",
  "ext_source_2",
  "ext_source_3",
  "commonarea_medi",
  "commonarea_mode",
  "elevators_medi",
  "elevators_mode",
  "entrances_mode",
  "entrances_medi",
  "floorsmax_mode",
  "floorsmax_medi",
  "floorsmin_mode",
  "floorsmin_medi",
  "landarea_mode",
  "landarea_medi",
  "livingarea_mode",
  "livingarea_medi",
  "nonlivingarea_mode",
  "nonlivingarea_medi",
  "livingapartments_mode",
  "livingapartments_medi",
  "nonlivingapartments_mode",
  "nonlivingapartments_medi",
  #
  "obs_30_cnt_social_circle",
  "def_30_cnt_social_circle",
  "obs_60_cnt_social_circle",
  "def_60_cnt_social_circle",
  "amt_req_credit_bureau_hour",
  "amt_req_credit_bureau_day",
  "amt_req_credit_bureau_week",
  "amt_req_credit_bureau_mon",
  "amt_req_credit_bureau_qrt",
  "amt_req_credit_bureau_year",
  
  "apartments_mode",
  "apartments_medi",
  "basementarea_mode",
  "basementarea_medi",
  "totalarea_mode",
  "years_beginexpluatation_mode",
  "years_beginexpluatation_medi",
  "years_build_mode",
  "years_build_medi",
  "days_last_phone_change",
  #
  # Engineered features.
  #
  "flag_not_employed",
  "days_employed_percent",
  "income_credit_percent",
  "income_per_person",
  "annuity_income_percent",
  #
  # Original features.
  #
  "name_contract_type",
  "code_gender",
  "flag_own_car",
  "flag_own_realty",
  "cnt_children", "amt_income_total",
  "name_type_suite",
  "name_income_type",
  "name_education_type",
  "name_family_status",
  "name_housing_type",
  "region_population_relative", "days_birth", "days_employed", "days_registration", "days_id_publish", "flag_mobil", "flag_work_phone", "flag_cont_mobile", "flag_phone", "flag_email",
  "occupation_type",
  "region_rating_client",
  "weekday_appr_process_start",
  "hour_appr_process_start", "reg_region_not_live_region", "reg_region_not_work_region", "live_region_not_work_region", "reg_city_not_live_city", "reg_city_not_work_city", "live_city_not_work_city",
  "organization_type",
  "fondkapremont_mode",
  "housetype_mode",
  "wallsmaterial_mode",
  "emergencystate_mode",
  "flag_document_2", "flag_document_3", "flag_document_4", "flag_document_5", "flag_document_6", "flag_document_7", "flag_document_8", "flag_document_9", "flag_document_10", "flag_document_11", "flag_document_12", "flag_document_13", "flag_document_14", "flag_document_15", "flag_document_16", "flag_document_17", "flag_document_18", "flag_document_19", "flag_document_20", "flag_document_21"
  #
  # Excluded because of correlation.
  #
  # "flag_emp_phone",
  # "amt_credit",
  # "region_rating_client_w_city",
  # "elevators_avg",
  # "commonarea_avg",
  # "entrances_avg",
  # "floorsmax_avg",
  # "floorsmin_avg",
  # "landarea_avg",
  # "livingarea_avg",
  # "nonlivingarea_avg",
  # "livingapartments_avg",
  # "nonlivingapartments_avg",
  # "basementarea_avg",
  # "apartments_avg",
  # "years_beginexpluatation_avg",
  # "years_build_avg",
)

# SELECT FEATURES -----------------------------------------------------------------------------------------------------

train <- data_train[, c("target", COLUMNS)]
test <- data_test[, COLUMNS]

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
