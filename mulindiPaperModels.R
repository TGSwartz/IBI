library(caret)
library(doMC)
registerDoMC(2)


#mulindi <- subset(mulindi, select = -c(cumulativeDays))

#trainControl <- trainControl(method = "repeatedcv", number = 5, repeats = 10)
trainControl <- trainControl(method = "cv", number = 10, returnResamp = "final")
set.seed(1)
fullRFFit <- train(yield ~ ., data = mulindi, method = "rf", 
                   trControl = trainControl, tuneLength = 10, metric = "Rsquared", ntree = 1000)
print(fullRFFit)

set.seed(1)
fullGLMFit <- train(yield ~ ., data = mulindi, method = "glmnet", 
                   trControl = trainControl, tuneLength = 10, 
                   preProcess = c("center", "scale"), metric = "Rsquared")
print(fullGLMFit)

set.seed(1)
fullLMFit <- train(yield ~ ., data = mulindi, method = "glm", 
                    trControl = trainControl, metric = "Rsquared")
print(fullLMFit)

set.seed(1)
plantRFFit <- train(yield ~ ., data = plantDta, method = "rf", 
                   trControl = trainControl, tuneLength = 10, metric = "Rsquared", ntree = 1000)
print(plantRFFit)

set.seed(1)
plantGLMFit <- train(yield ~ ., data = plantDta, method = "glmnet", 
                    trControl = trainControl, tuneLength = 10, 
                    preProcess = c("center", "scale"), metric = "Rsquared")
print(plantGLMFit)

set.seed(1)
plantLMFit <- train(yield ~ ., data = plantDta, method = "glm", 
                    trControl = trainControl, metric = "Rsquared")
print(plantLMFit)

set.seed(1)
satRFFit <- train(yield ~ ., data = satDta, method = "rf", 
                   trControl = trainControl, tuneLength = 10, metric = "Rsquared", ntree = 1000)
print(satRFFit)

set.seed(1)
satGLMFit <- train(yield ~ ., data = satDta, method = "glmnet", 
                    trControl = trainControl, tuneLength = 10, 
                    preProcess = c("center", "scale"), metric = "Rsquared")
print(satGLMFit)

set.seed(1)
satLMFit <- train(yield ~ ., data = satDta, method = "glm", 
                    trControl = trainControl, metric = "Rsquared")
print(satLMFit)

set.seed(1)
satCompCaseRFFit <- train(yield ~ ., data = satDta[complete.cases(plantDta), ], method = "rf", 
                  trControl = trainControl, tuneLength = 10, metric = "Rsquared", ntree = 1000)
print(satCompCaseRFFit)

set.seed(1)
satCompCaseGLMFit <- train(yield ~ ., data = satDta[complete.cases(plantDta), ], method = "glmnet", 
                   trControl = trainControl, tuneLength = 10, 
                   preProcess = c("center", "scale"), metric = "Rsquared")
print(satCompCaseGLMFit)

set.seed(1)
satCompCaseLMFit <- train(yield ~ ., data = satDta[complete.cases(plantDta), ], method = "glm", 
                  trControl = trainControl, metric = "Rsquared")
print(satCompCaseLMFit)


timeOnlyCol <- c("month", "weekday", "yield")

set.seed(1)
fullTimeOnlyRFFit <- train(yield ~ ., data = subset(mulindi, select = timeOnlyCol), method = "rf", 
                   trControl = trainControl, tuneLength = 10, metric = "Rsquared", ntree = 1000)
print(fullTimeOnlyRFFit)

set.seed(1)
fullTimeOnlyGLMFit <- train(yield ~ ., data = subset(mulindi, select = timeOnlyCol), method = "glmnet", 
                    trControl = trainControl, tuneLength = 10, 
                    preProcess = c("center", "scale"), metric = "Rsquared")
print(fullTimeOnlyGLMFit)

set.seed(1)
fullTimeOnlyLMFit <- train(yield ~ ., data = subset(mulindi, select = timeOnlyCol), method = "glm", 
                   trControl = trainControl, metric = "Rsquared")
print(fullTimeOnlyLMFit)

resamps <- resamples(list(fullRFFit = fullRFFit,
                          fullGLMFit = fullGLMFit,
                          fullLMFit = fullLMFit,
                          plantRFFit = plantRFFit,
                          plantGLMFit = plantGLMFit,
                          plantLMFit = plantLMFit,
                          satRFFit = satRFFit,
                          satGLMFit = satGLMFit,
                          satLMFit = satLMFit,
                          satCompCaseRFFit = satCompCaseRFFit,
                          satCompCaseGLMFit = satCompCaseGLMFit,
                          satCompCaseLMFit = satCompCaseLMFit,
                          fullTimeOnlyRFFit = fullTimeOnlyRFFit,
                          fullTimeOnlyGLMFit = fullTimeOnlyGLMFit,
                          fullTimeOnlyLMFit = fullTimeOnlyLMFit))

save(resamps, file = "/Users/Tom/Documents/IBI/modelResamples.RData")

saveList <- c("fullRFFit", "fullGLMFit", "fullLMFit", "plantRFFit", "plantGLMFit", "plantLMFit", 
              "satRFFit", "satGLMFit", "satLMFit", "satCompCaseRFFit", "satCompCaseGLMFit", 
              "satCompCaseLMFit", "fullTimeOnlyRFFit", "fullTimeOnlyGLMFit", "fullTimeOnlyLMFit")
save(list = saveList, file = "/Users/Tom/Documents/IBI/mulindiModels.RData")