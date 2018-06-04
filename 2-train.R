train <- data_train %>%
  select(target, name_contract_type, code_gender, flag_own_car, flag_own_realty, cnt_children, amt_income_total, amt_credit, name_type_suite, name_income_type, name_education_type, name_family_status, name_housing_type, region_population_relative, days_birth, days_employed, days_registration, days_id_publish, flag_mobil, flag_emp_phone, flag_work_phone, flag_cont_mobile, flag_phone, flag_email, occupation_type, region_rating_client, region_rating_client_w_city, weekday_appr_process_start, hour_appr_process_start, reg_region_not_live_region, reg_region_not_work_region, live_region_not_work_region, reg_city_not_live_city, reg_city_not_work_city, live_city_not_work_city, organization_type, fondkapremont_mode, housetype_mode, wallsmaterial_mode, emergencystate_mode, flag_document_2, flag_document_3, flag_document_4, flag_document_5, flag_document_6, flag_document_7, flag_document_8, flag_document_9, flag_document_10, flag_document_11, flag_document_12, flag_document_13, flag_document_14, flag_document_15, flag_document_16, flag_document_17, flag_document_18, flag_document_19, flag_document_20, flag_document_21)
test <- data_test %>%
  select(name_contract_type, code_gender, flag_own_car, flag_own_realty, cnt_children, amt_income_total, amt_credit, name_type_suite, name_income_type, name_education_type, name_family_status, name_housing_type, region_population_relative, days_birth, days_employed, days_registration, days_id_publish, flag_mobil, flag_emp_phone, flag_work_phone, flag_cont_mobile, flag_phone, flag_email, occupation_type, region_rating_client, region_rating_client_w_city, weekday_appr_process_start, hour_appr_process_start, reg_region_not_live_region, reg_region_not_work_region, live_region_not_work_region, reg_city_not_live_city, reg_city_not_work_city, live_city_not_work_city, organization_type, fondkapremont_mode, housetype_mode, wallsmaterial_mode, emergencystate_mode, flag_document_2, flag_document_3, flag_document_4, flag_document_5, flag_document_6, flag_document_7, flag_document_8, flag_document_9, flag_document_10, flag_document_11, flag_document_12, flag_document_13, flag_document_14, flag_document_15, flag_document_16, flag_document_17, flag_document_18, flag_document_19, flag_document_20, flag_document_21)

fit <- glm(as.factor(target) ~ ., data = train, family = binomial())

test$target <- predict(fit, test)

submission = cbind(SK_ID_CURR = data_test$sk_id_curr, TARGET = predict(fit, test, type = "response")) %>%
  data.frame() %>%
  #
  # This is necessary to ensure that output not converted to scientific notation.
  #
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR))

write.csv(submission, "test-submission.csv", quote = FALSE, row.names = FALSE)
