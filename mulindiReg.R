libraries <- c("caret")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

source('~/Github/IBI/bimonthClean.R')

#trCtrl <- trainControl(method = "repeatedcv", number = 3, repeats = 10)
trCtrl <- trainControl(method = "cv", number = 10)

set.seed(100)
rfGrid <- data.frame(mtry = seq(1, 8, by = 1))
rfFit <- train(yield ~ ., data = mulindiTrainDf, tuneGrid = rfGrid,
            method = "rf", metric = "Rsquared", trControl = trCtrl)
print(rfFit)

set.seed(100)
glmnetGrid <- expand.grid(alpha = seq(0, 1, by = .25), lambda = seq(0, 3000, by = 300)) # lambda roughly based off automatically computed values
glmFit <- train(yield ~ ., data = mulindiTrainDf, 
            method = "glmnet", trControl = trCtrl, tuneGrid = glmnetGrid,
          preProcess = c("center", "scale"), metric = "Rsquared")
print(glmFit)

set.seed(100)
lmFit <- train(yield ~ ., data = mulindiTrainDf, 
                method = "lm", trControl = trCtrl, metric = "Rsquared")
print(lmFit)