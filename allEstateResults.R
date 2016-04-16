##########################################
### Final code to run that produces    ###
### results and prediction metrics     ###
##########################################

# load libraries

library(caret)
library(doMC)
registerDoMC(2)
library(stringr)
library(geosphere)

# source files (shagasha and sorwate will create new dataframes)

source('~/Github/IBI/fullMulindiClean.R')
source('~/Github/IBI/shagashaClean.R')
source('~/Github/IBI/sorwateClean.R')

# create sorwathe area dataframes

sorwathe <- CreateData("Sorwathe", recordedWeather = T)
cyohoha <- CreateData("Cyohoha", recordedWeather = T)
rukeri <- CreateData("Rukeri", recordedWeather = T)
assopthe <- CreateData("Assopthe", recordedWeather = F)

# define the function that will take in data and produce prediction results
# it will take data, can take a predefined model or fit a new random forest or GLMNet
# it will train (10 fold CV) on the data up until the prediction year then compute accuracy on the remaining data
# if no model is provided, the user must input either "rf" or "glmnet", it will default to "glmnet"

FutureModelPred <- function(data, providedModel = T, model = NULL, startPredYear = 2014, modelType = "glmnet") {

  # the model will be trained using 10 fold CV and the same random seed
  # tuning parameter grids are set for both model types
  # R2 is used to optimize
  # year is excluded as it is not a variable that can be depended on for the future
  
  if(providedModel == F & modelType == "rf") {
    trainControl <- trainControl(method = "cv", number = 10,returnResamp = "final") 
    tuneGrid <- data.frame(mtry = ceiling(seq(1, ncol(data), length.out = 5)))
    set.seed(100)
    model <- train(yield ~ ., data = subset(data[as.numeric(as.character(data$year)) < startPredYear, ], select = -c(year)), method = "rf", 
                   trControl = trainControl, tuneGrid = tuneGrid, metric = "Rsquared", ntree = 1000)
  } else if (providedModel == F & modelType == "glmnet") {
    trainControl <- trainControl(method = "cv", number = 10,returnResamp = "final") 
    set.seed(100)
    model <- train(yield ~ ., data = subset(data[as.numeric(as.character(data$year)) < startPredYear, ], select = -c(year)), method = "glmnet", 
                   trControl = trainControl, metric = "Rsquared", preProcess = c("center", "scale"))
  } else {
    model = model
  }
  
  # set data as the training data as up until the prediction year (exclusive)
  # remove August 2014 which has some missing data for some of the data
  # removing the missing data improves comparability
  
  data <- data[as.numeric(as.character(data$year)) >= startPredYear, ]
  data <- data[!(as.numeric(as.character(data$month)) ==8 & as.numeric(as.character(data$year)) == 2014), ]
  
  # fill a vector for predictions
  # predict for complete cases of features (NAs will be left for non-completes)
  # convert to a dataframe with time variables
  
  pred <- rep(NA, nrow(data))
  pred[complete.cases(subset(data, select = -c(yield)))] <- suppressWarnings(predict(model, data))
  pred <- data.frame(month = data$month, year = data$year, week = data$week, 
                     yield = pred)
  
  # aggregate predictions and true data for comparisions
  
  trueWeek <- aggregate(yield ~ week + year, data = data, 
                        FUN = mean, na.rm = T, na.action = NULL )
  trueMonth <- aggregate(yield ~ month + year, data = data, 
                         FUN = mean, na.rm = T, na.action = NULL )
  predWeek <- aggregate(yield ~ week + year, data = pred, 
                        FUN = mean, na.rm = T, na.action = NULL )
  predMonth <- aggregate(yield ~ month + year, data = pred, 
                         FUN = mean, na.rm = T, na.action = NULL )
  
  # compute RMSE and R2 for the different granularities and print results
  # return a list of results and data and the model fit
  
  rmseDay <- RMSE(pred$yield, data$yield, na.rm = T)
  rmseWeek <- RMSE(predWeek$yield, trueWeek$yield, na.rm = T)
  rmseMonth <- RMSE(predMonth$yield, trueMonth$yield, na.rm = T)
  rsqDay <- R2(pred$yield, data$yield, na.rm = T, formula = "corr")
  rsqWeek <- R2(predWeek$yield, trueWeek$yield, na.rm = T, formula = "corr")
  rsqMonth <- R2(predMonth$yield, trueMonth$yield,  na.rm = T, formula = "corr")
  
  cat(paste("Plantation Name:", comment(data), modelType), "\n")
  cat(paste("Daily RMSE is:", as.numeric(rmseDay)), "\n")
  cat(paste("Weekly RMSE is:", as.numeric(rmseWeek)), "\n")
  cat(paste("Monthly RMSE is:", as.numeric(rmseMonth)), "\n")
  cat(paste("Daily R-Squared is:", as.numeric(rsqDay)), "\n")
  cat(paste("Weekly R-Squared is:", as.numeric(rsqWeek)), "\n")
  cat(paste("Monthly R-Squared is:", as.numeric(rsqMonth)), "\n")
  cat("\n")
  
  return(list(rmseDay = rmseDay, rmseWeek = rmseWeek, rmseMonth = rmseMonth, 
              rsqDay = rsqDay, rsqWeek = rsqWeek, rsqMonth = rsqMonth,
              predDay = pred, predWeek = predWeek, predMonth = predMonth,
              trueDay = data, trueWeek = trueWeek, trueMonth = trueMonth,
              fit = model))
}

# compute model fits and results for GLMNet models
# RF models not run but results are worse than GLMNet
# RF models included only as a robustness check

mul2014SatGLM <- FutureModelPred(data = mulSatDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
sha2014SatGLM <- FutureModelPred(data = shagSatDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
mul2014EstGLM <- FutureModelPred(data = mulPlantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
cyo2014SatGLM <- FutureModelPred(data = cyohoha$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
sor2014SatGLM <- FutureModelPred(data = sorwathe$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
cyo2014EstGLM <- FutureModelPred(data = cyohoha$plantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
sor2014EstGLM <- FutureModelPred(data = sorwathe$plantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
ruk2014SatGLM <- FutureModelPred(data = rukeri$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
ruk2014EstGLM <- FutureModelPred(data = rukeri$plantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
ass2014SatGLM <- FutureModelPred(data = assopthe$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")

# mul2014SatRF <- FutureModelPred(data = mulSatDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# sha2014SatRF <- FutureModelPred(data = shagSat, providedModel = F, startPredYear = 2014, modelType = "rf")
# mul2014EstRF <- FutureModelPred(data = mulPlantDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# #shag2014PlantResults <- FutureModelPred(data = shagPlant, providedModel = F, startPredYear = 2014)
# cyo2014SatRF <- FutureModelPred(data = cyohoha$satDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# sor2014SatRF <- FutureModelPred(data = sorwathe$satDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# cyo2014EstRF <- FutureModelPred(data = cyohoha$plantDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# sor2014EstRF <- FutureModelPred(data = sorwathe$plantDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# ruk2014SatRF <- FutureModelPred(data = rukeri$satDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# ruk2014EstRF <- FutureModelPred(data = rukeri$plantDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# #rug2014SatRF <- FutureModelPred(data = rugundo$satDta, providedModel = F, startPredYear = 2014, modelType = "rf")
# ass2014SatRF <- FutureModelPred(data = assopthe$satDta, providedModel = F, startPredYear = 2014, modelType = "rf")

# save a list of all GLM and RF models

saveList = c(ls(pattern = "RF"), ls(pattern = "GLM"))
save(list = saveList, file = "/Users/Tom/Documents/IBI/fullEstateModelsNAisNAnoRF.RData")

# create dataframes that will contain results of model predictions on holdout set
# not a particularly elegant solution

estateNames = c("Mulindi", "Shagasha", "Rukeri", "Cyohoha", "Assopthe", "Sorwathe", "Average")
rowNames = c("Factory Weather Station E-Net", "Satellite Composite E-Net")
df <- data.frame(matrix(ncol = length(estateNames), nrow = length(rowNames)))
colnames(df) <- estateNames
rownames(df) <- rowNames

dayRsq <- weekRsq <- monthRsq <- dayRmse <- weekRmse <- monthRmse <- df

# for each of the fits in the saveList, pull the estate name, 
# the data type (estate or satellite), and GLM or RF
# then put the results into the approriate dataframes

for (i in 1:length(saveList)) {
  plant <- substr(saveList[i], 1, 3)
  plant <- estateNames[grep(pattern = plant, x = estateNames, ignore.case = T)]
  dataType <- substr(saveList[i], 8, 10)
  modelType <- substr(saveList[i], 11, 12)
  if (modelType == "GL" & dataType == "Sat") {
    dayRsq["Satellite Composite E-Net", plant] <-eval(parse(text=paste(saveList[i], "$rsqDay", sep = "")))
    weekRsq["Satellite Composite E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rsqWeek", sep = "")))
    monthRsq["Satellite Composite E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rsqMonth", sep = "")))
  } else if (modelType == "GL" & dataType == "Est") {
    dayRsq["Factory Weather Station E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rsqDay", sep = "")))
    weekRsq["Factory Weather Station E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rsqWeek", sep = "")))
    monthRsq["Factory Weather Station E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rsqMonth", sep = "")))
  } else if (modelType == "RF" & dataType == "Sat") {
    dayRsq["Satellite Composite Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rsqDay", sep = "")))
    weekRsq["Satellite Composite Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rsqWeek", sep = "")))
    monthRsq["Satellite Composite Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rsqMonth", sep = "")))
  } else if (modelType == "RF" & dataType == "Est") {
    dayRsq["Factory Weather Station Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rsqDay", sep = "")))
    weekRsq["Factory Weather Station Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rsqWeek", sep = "")))
    monthRsq["Factory Weather Station Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rsqMonth", sep = "")))
  }
  
  if (modelType == "GL" & dataType == "Sat") {
    dayRmse["Satellite Composite E-Net", plant] <-eval(parse(text=paste(saveList[i], "$rmseDay", sep = "")))
    weekRmse["Satellite Composite E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rmseWeek", sep = "")))
    monthRmse["Satellite Composite E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rmseMonth", sep = "")))
  } else if (modelType == "GL" & dataType == "Est") {
    dayRmse["Factory Weather Station E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rmseDay", sep = "")))
    weekRmse["Factory Weather Station E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rmseWeek", sep = "")))
    monthRmse["Factory Weather Station E-Net", plant] <- eval(parse(text=paste(saveList[i], "$rmseMonth", sep = "")))
  } else if (modelType == "RF" & dataType == "Sat") {
    dayRmse["Satellite Composite Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rmseDay", sep = "")))
    weekRmse["Satellite Composite Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rmseWeek", sep = "")))
    monthRmse["Satellite Composite Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rmseMonth", sep = "")))
  } else if (modelType == "RF" & dataType == "Est") {
    dayRmse["Factory Weather Station Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rmseDay", sep = "")))
    weekRmse["Factory Weather Station Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rmseWeek", sep = "")))
    monthRmse["Factory Weather Station Random Forest", plant] <- eval(parse(text=paste(saveList[i], "$rmseMonth", sep = "")))
  }
}

# compute a final average and round to 2 decimals

dayRsq$Average <- rowMeans(dayRsq[, 1:(ncol(dayRsq-1))], na.rm = T)
weekRsq$Average <- rowMeans(weekRsq[, 1:(ncol(weekRsq-1))], na.rm = T)
monthRsq$Average <- rowMeans(monthRsq[, 1:(ncol(monthRsq-1))], na.rm = T)

dayRsq = round(dayRsq, digits = 2)
weekRsq = round(weekRsq, digits = 2)
monthRsq = round(monthRsq, digits = 2)

write.csv(dayRsq, file = "/Users/Tom/Documents/IBI/finalData/dayRsqResults.csv", row.names = T)
write.csv(weekRsq, file = "/Users/Tom/Documents/IBI/finalData/weekRsqResults.csv", row.names = T)
write.csv(monthRsq, file = "/Users/Tom/Documents/IBI/finalData/monthRsqResults.csv", row.names = T)

# define function to create a dataframe of coefficients from the best model for each dataset
# remove week, month, and intercept as these are not relevant for comparing different satellite products

CoefDf <- function(model, rankOnly = F) {
  coef <- as.matrix(coef(model$finalModel, model$bestTune$lambda)) # select best model coef
  coef <- abs(round(coef[, 1], 3)) # round to 3 digits
  coef <- coef[order(abs(coef), decreasing = T)] # sort in decreasing order
  coef <- coef[!grepl("week", x = names(coef))]
  coef <- coef[!grepl("month", x = names(coef))]
  coef <- coef[!grepl("Intercept", x = names(coef))]
  coefDf <- data.frame(rank(coef, ties.method = "first"), row.names = NULL)
  coefDf$names <- names(coef)
  return(coefDf)
}

# find all satellite GLM fits to pull put their coefficients and then run them through the
# function that determines best coefficients

satList = c(ls(pattern = "SatGLM"))
for (i in 1:length(satList)) {
  assign(paste(satList[i], "coef", sep = ""), CoefDf(eval(
    parse(text = paste(satList[i])))$fit))
}

# define list of fits and their coefficients

coefList <- list(ruk2014SatGLMcoef, ass2014SatGLMcoef, cyo2014SatGLMcoef, mul2014SatGLMcoef, sha2014SatGLMcoef, sor2014SatGLMcoef)

# function to merge all the cofficients dataframes together

mergeAll <- function(x, y) {
  suppressWarnings(merge(x, y, all=TRUE, by="names"))
}

# merge and average the coefficients from all the different estates
# then order them by absolute value and rank them 
# pull out the rank for description purposes

coefDf <- Reduce(mergeAll, coefList)
coefDf$average <- rowMeans(subset(coefDf, select = -c(names)))
coefDf <- coefDf[order(abs(coefDf$average), decreasing = T), ]
coefDf$rank <- rank(-coefDf$average)
coefDf <- subset(coefDf, select = c(names, rank))

write.csv(coefDf, "/Users/Tom/Documents/IBI/finalData/coefAvg.csv", row.names = F)