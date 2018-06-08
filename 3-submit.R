TIME <- format(Sys.time(), "%Y%m%d-%H%M%S")

sink(sprintf("%s.txt", TIME))
print(fit)
cat("\n")
fit$results %>% arrange(desc(ROC))
sink()

saveRDS(fit, file = sprintf("%s.rds", TIME))

submission = cbind(SK_ID_CURR = data_test$sk_id_curr, TARGET = predict(fit, test, type = "prob")) %>%
  data.frame() %>%
  #
  # This is necessary to ensure that output not converted to scientific notation.
  #
  mutate(SK_ID_CURR = as.integer(SK_ID_CURR))

write.csv(submission, sprintf("%s.csv", TIME), quote = FALSE, row.names = FALSE)
