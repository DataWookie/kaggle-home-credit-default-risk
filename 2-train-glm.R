fit <- train(target ~ ., data = train, method = "glm",
                 metric = "ROC",
                 trControl = trainControl(
                   method = "cv",
                   number = 10,
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary,
                   verboseIter = TRUE
                 ))
