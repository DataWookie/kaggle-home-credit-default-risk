submission = cbind(SK_ID_CURR = data_test$sk_id_curr, TARGET = predict(fit, test, type = "response")) %>%
  data.frame() %>%
  #
  # This is necessary to ensure that output not converted to scientific notation.
  #
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR))

write.csv(submission, "test-submission.csv", quote = FALSE, row.names = FALSE)