# check yield correlations

rsq = function(y,f) { 1 - sum((y-f)^2)/sum((y-mean(y))^2) }

library(Metrics)
library(caret)

trainControl <- trainControl(method = "cv", number = 10, returnResamp = "final")

cor(mulindi$yield, shagasha$yield, use = "complete.obs")
cor(mulindiWeek$yield, shagashaWeek$yield, use = "complete.obs")
cor(mulindiMonth$yield, shagashaMonth$yield, use = "complete.obs")

rmse(satDta[complete.cases(satDta), ]$yield, predict(satRFFit, satDta[complete.cases(satDta), ]))
rmse(shagasha[complete.cases(shagasha), ]$yield, predict(satRFFit, shagasha[complete.cases(shagasha), ]))

set.seed(1)
satWeekRFFit <- train(yield ~ ., data = satWeekDta, method = "rf", 
                   trControl = trainControl, tuneLength = 5, metric = "Rsquared", 
                   ntree = 500)

set.seed(1)
satWeekGLMFit <- train(yield ~ ., data = satWeekDta, method = "glmnet", 
                    trControl = trainControl, tuneLength = 5, 
                    #preProcess = c("center", "scale"), 
                    metric = "Rsquared")

set.seed(1)
satMonthRFFit <- train(yield ~ ., data = satMonthDta, method = "rf", 
                       trControl = trainControl, tuneLength = 5, metric = "Rsquared", 
                       ntree = 500)

set.seed(1)
satMonthGLMFit <- train(yield ~ ., data = satMonthDta, method = "glmnet", 
                    trControl = trainControl, tuneLength = 5, 
                    preProcess = c("center", "scale"), metric = "Rsquared")

satWeekLMFit <- train(yield ~ ., data = satWeekDta, method = "glm", 
                        trControl = trainControl, 
                        #preProcess = c("center", "scale"), 
                      metric = "Rsquared")

rmse(satWeekDta[complete.cases(satWeekDta), ]$yield, predict(satWeekRFFit, satWeekDta[complete.cases(satWeekDta), ]))
rmse(shagashaWeek[complete.cases(shagashaWeek), ]$yield, predict(satWeekRFFit, shagashaWeek[complete.cases(shagashaWeek), ]))

rmse(satMonthDta[complete.cases(satMonthDta), ]$yield, predict(satMonthRFFit, satMonthDta[complete.cases(satMonthDta), ]))
rmse(shagashaMonth[complete.cases(shagashaMonth), ]$yield, predict(satMonthRFFit, shagashaMonth[complete.cases(shagashaMonth), ]))

Rsquared <- function(model, inputs, output) {
  SSr <- (predict(model, inputs[complete.cases(inputs), ]) - mean(output[complete.cases(inputs)]))^2
  #print(predict(model, inputs))
  #print((output[complete.cases(inputs)] - mean(output[complete.cases(inputs)])))
  SSr <- sum(SSr)
  SSt <- sum((output[complete.cases(inputs)] - mean(output[complete.cases(inputs)]))^2)
  SSe <- sum((output[complete.cases(inputs)] - predict(model, inputs[complete.cases(inputs), ]))^2)
  print(SSe)
  print(SSr)
  print(SSt)
  print(SSr/SSt)
  print(1- SSe/SSt)
}

rmse(satWeekDta[complete.cases(satWeekDta), ]$yield, predict(satWeekRFFit, satWeekDta[complete.cases(satWeekDta), ]))
rmse(shagashaWeek[complete.cases(subset(shagashaWeek, select = -c(plantationRainfall))), -2]$yield, predict(satWeekRFFit, shagashaWeek[complete.cases(subset(shagashaWeek, select = -c(plantationRainfall))), -2]))

rmse(satMonthDta[complete.cases(satMonthDta), ]$yield, predict(satMonthRFFit, satMonthDta[complete.cases(satMonthDta), ]))
rmse(shagashaMonth[complete.cases(subset(shagashaMonth, select = -c(plantationRainfall))), -2]$yield, 
     predict(satMonthRFFit, shagashaMonth[complete.cases(subset(shagashaMonth, 
                                                                 select = -c(plantationRainfall))), -2]))
