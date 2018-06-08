COLUMNS = c(
  "amt_annuity",
  "amt_goods_price",
  # "own_car_age",
  # "cnt_fam_members",
  # "ext_source_1",
  # "ext_source_2",
  # "ext_source_3",
  # "apartments_avg",
  # "basementarea_avg",
  # "years_beginexpluatation_avg",
  # "years_build_avg",
  # "commonarea_avg",
  # "elevators_avg",
  # "entrances_avg",
  # "floorsmax_avg",
  # "floorsmin_avg",
  # "landarea_avg",
  # "livingapartments_avg",
  # "livingarea_avg",
  # "nonlivingapartments_avg",
  # "nonlivingarea_avg",
  # "apartments_mode",
  # "basementarea_mode",
  # "years_beginexpluatation_mode",
  # "years_build_mode",
  # "commonarea_mode",
  # "elevators_mode",
  # "entrances_mode",
  # "floorsmax_mode",
  # "floorsmin_mode",
  # "landarea_mode",
  # "livingapartments_mode",
  # "livingarea_mode",
  # "nonlivingapartments_mode",
  # "nonlivingarea_mode",
  # "apartments_medi",
  # "basementarea_medi",
  # "years_beginexpluatation_medi",
  # "years_build_medi",
  # "commonarea_medi",
  # "elevators_medi",
  # "entrances_medi",
  # "floorsmax_medi",
  # "floorsmin_medi",
  # "landarea_medi",
  # "livingapartments_medi",
  # "livingarea_medi",
  # "nonlivingapartments_medi",
  # "nonlivingarea_medi",
  # "totalarea_mode",
  # "obs_30_cnt_social_circle",
  # "def_30_cnt_social_circle",
  # "obs_60_cnt_social_circle",
  # "def_60_cnt_social_circle",
  # "days_last_phone_change",
  # "amt_req_credit_bureau_hour",
  # "amt_req_credit_bureau_day",
  # "amt_req_credit_bureau_week",
  # "amt_req_credit_bureau_mon",
  # "amt_req_credit_bureau_qrt",
  # "amt_req_credit_bureau_year",
  #
  # Engineered features.
  #
  # INSERT HERE
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
)

# SELECT FEATURES -----------------------------------------------------------------------------------------------------

train <- data_train[, c("target", COLUMNS)]
test <- data_test[, COLUMNS]

# IDENTIFY COLUMNS TO REMOVE ------------------------------------------------------------------------------------------

nearZeroVar(train)
#
train_numeric = train[, sapply(train, class) != "factor"]
#
findCorrelation(cor(train_numeric), cutoff = 0.95)
findLinearCombos(train_numeric)
#
rm(train_numeric)

# DOWNSAMPLE ----------------------------------------------------------------------------------------------------------

# train <- rbind(
#   train %>% filter(target == "yes") %>% sample_n(15000),
#   train %>% filter(target == "no") %>% sample_n(15000)
# )
