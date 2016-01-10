library(caret)
library(doMC)
registerDoMC(2)

#mulindi <- subset(mulindi, select = -c(week))
#satDta <- subset(satDta, select = -c(week))

timeOnlyCol <- c("month", "weekday", "week", "yield")
timeOnlyDta <- subset(mulindi, select = timeOnlyCol)

mulindi <- subset(mulindi, select = -c(week))
satDta <- subset(satDta, select = -c(week))
plantDta <- subset(plantDta, select = -c(week))

#mulindi <- subset(mulindi, select = -c(cumulativeDays))

#trainControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, returnResamp = "final")
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

set.seed(1)
fullTimeOnlyRFFit <- train(yield ~ ., data = timeOnlyDta, method = "rf", 
                   trControl = trainControl, tuneLength = 10, metric = "Rsquared", ntree = 1000)
print(fullTimeOnlyRFFit)

set.seed(1)
fullTimeOnlyGLMFit <- train(yield ~ ., data = timeOnlyDta, method = "glmnet", 
                    trControl = trainControl, tuneLength = 10, 
                    preProcess = c("center", "scale"), metric = "Rsquared")
print(fullTimeOnlyGLMFit)

set.seed(1)
fullTimeOnlyLMFit <- train(yield ~ ., data = timeOnlyDta, method = "glm", 
                   trControl = trainControl, metric = "Rsquared")
print(fullTimeOnlyLMFit)

resamps <- resamples(list(fullRFFit = fullRFFit,
                          fullEnetFit = fullGLMFit,
                          fullLMFit = fullLMFit,
                          plantRFFit = plantRFFit,
                          plantEnetFit = plantGLMFit,
                          plantLMFit = plantLMFit,
                          satRFFit = satRFFit,
                          satEnetFit = satGLMFit,
                          satLMFit = satLMFit,
                          satCompCaseRFFit = satCompCaseRFFit,
                          satCompCaseEnetFit = satCompCaseGLMFit,
                          satCompCaseLMFit = satCompCaseLMFit,
                          fullTimeOnlyRFFit = fullTimeOnlyRFFit,
                          fullTimeOnlyEnetFit = fullTimeOnlyGLMFit,
                          fullTimeOnlyLMFit = fullTimeOnlyLMFit),
                     modelNames = c("Full Random Forest", "Full Elastic Net", "Full OLS",
                                    "Estate-Only Random Forest", "Estate-Only Elastic Net", "Estate-Only OLS",
                                    "Satellite-Only Random Forest", "Satellite-Only Elastic Net", "Satellite-Only OLS",
                                    "Satellite Complete Observations Random Forest", "Satellite Complete Observations Elastic Net", "Satellite Complete Obs OLS",
                                    "Time Variables-Only Random Forest", "Time Variables-Only Elastic Net", "Time Variables-Only OLS"))

save(resamps, file = "/Users/Tom/Documents/IBI/modelResamples.RData")

set.seed(1)
satWeekRFFit <- train(yield ~ ., data = satWeekDta, method = "rf", 
                      trControl = trainControl, tuneLength = 10, metric = "Rsquared", 
                      ntree = 1000)

set.seed(1)
satWeekGLMFit <- train(yield ~ ., data = satWeekDta, method = "glmnet", 
                       trControl = trainControl, tuneLength = 10, 
                       preProcess = c("center", "scale"), 
                       metric = "Rsquared")

set.seed(1)
satMonthRFFit <- train(yield ~ ., data = satMonthDta, method = "rf", 
                       trControl = trainControl, tuneLength = 10, metric = "Rsquared", 
                       ntree = 1000)

set.seed(1)
satMonthGLMFit <- train(yield ~ ., data = satMonthDta, method = "glmnet", 
                        trControl = trainControl, tuneLength = 10, 
                        preProcess = c("center", "scale"), metric = "Rsquared")

timeResamps <- resamples(list(satWeekRFFit = satWeekRFFit,
                              satWeekEnetFit = satWeekGLMFit,
                              satMonthRFFit = satMonthRFFit,
                              satMonthEnetFit = satMonthGLMFit),
                         modelNames = c("Satellite Weekly Random Forest", 
                                        "Satellite Weekly Elastic Net", 
                                        "Satellite Monthly Random Forest", 
                                        "Satellite Monthly Elastic Net"))

saveList <- c("fullRFFit", "fullGLMFit", "fullLMFit", "plantRFFit", "plantGLMFit", "plantLMFit", 
               "satRFFit", "satGLMFit", "satLMFit", "satCompCaseRFFit", "satCompCaseGLMFit", 
               "satCompCaseLMFit", "fullTimeOnlyRFFit", "fullTimeOnlyGLMFit", "fullTimeOnlyLMFit",
               "satWeekRFFit", "satWeekGLMFit", "satMonthRFFit", "satMonthGLMFit")
save(list = saveList, file = "/Users/Tom/Documents/IBI/mulindiModels.RData")

# mulindiX <- subset(mulindi[!is.na(mulindi$yield),], select = -c(yield, month, weekday, week, weekend))
# tooHighCor <- findCorrelation(cor(mulindiX, use = "complete.obs"), cutoff = .75)
# mulindiLowCor <- mulindiX[, -tooHighCor]
# preProc <- preProcess(mulindiLowCor, method = c("center", "scale"))
# mulindiLowCor <- predict(preProc, mulindiLowCor)
# mulindiLowCor <- cbind(mulindiLowCor, subset(mulindi[!is.na(mulindi$yield),], select = c(month, weekday, week, weekend, yield)))
# #mulindiLowCor$yield <- mulindi$yield
# 
# nnetFit <- avNNet(mulindiLowCor, mulindiLowCor$yield, 
#                   linout = T, trace = F, maxit = 500, size = 5, decay = .1,
#                   maxNWts = 10 * (ncol(mulindiLowCor) + 1) + 10 + 1)