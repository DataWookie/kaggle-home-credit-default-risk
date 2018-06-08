fit <- train(as.factor(target) ~ ., data = train, method = "svmRadial",
                   metric = "ROC",
                   trControl = trainControl(
                     method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary,
                     verboseIter = TRUE
                   ))
