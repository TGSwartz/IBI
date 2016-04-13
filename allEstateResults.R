# load libraries

library(caret)
library(doMC)
registerDoMC(2)
library(stringr)
library(geosphere)

mulIncomeTrue <- F
shagIncomeTrue <- F
source('~/Github/IBI/fullMulindiClean.R')
source('~/Github/IBI/shagashaClean.R')
source('~/Github/IBI/sorwateClean.R')

sorwathe <- CreateData("Sorwathe", recordedWeather = T)
cyohoha <- CreateData("Cyohoha", recordedWeather = T)
rukeri <- CreateData("Rukeri", recordedWeather = T)
assopthe <- CreateData("Assopthe", recordedWeather = F)
#rugundo <- CreateData("Rugundo", recordedWeather = F)

FutureModelPred <- function(data, providedModel = T, model = NULL, startPredYear = 2014, modelType = "rf") {

  
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
  
  #data <- data[complete.cases(subset(mulPlantDta, select = -c(yield))), ]
  
  
  data <- data[as.numeric(as.character(data$year)) >= startPredYear, ]
  data <- data[!(as.numeric(as.character(data$month)) ==8 & as.numeric(as.character(data$year)) == 2014), ]
  pred <- rep(NA, nrow(data))
  pred[complete.cases(subset(data, select = -c(yield)))] <- suppressWarnings(predict(model, data))
  pred <- data.frame(month = data$month, year = data$year, week = data$week, 
                     yield = pred)
  trueWeek <- aggregate(yield ~ week + year, data = data, 
                        FUN = mean, na.rm = T, na.action = NULL )
  trueMonth <- aggregate(yield ~ month + year, data = data, 
                         FUN = mean, na.rm = T, na.action = NULL )
  predWeek <- aggregate(yield ~ week + year, data = pred, 
                        FUN = mean, na.rm = T, na.action = NULL )
  predMonth <- aggregate(yield ~ month + year, data = pred, 
                         FUN = mean, na.rm = T, na.action = NULL )
  rmseDay <- RMSE(pred$yield, data$yield, na.rm = T)
  rmseWeek <- RMSE(predWeek$yield, trueWeek$yield, na.rm = T)
  rmseMonth <- RMSE(predMonth$yield, trueMonth$yield, na.rm = T)
  rsqDay <- R2(pred$yield, data$yield, na.rm = T, formula = "corr")
  rsqWeek <- R2(predWeek$yield, trueWeek$yield, na.rm = T, formula = "corr")
  rsqMonth <- R2(predMonth$yield, trueMonth$yield,  na.rm = T, formula = "corr")
  
  print(paste("Plantation Name:", comment(data), modelType))
  print(paste("Daily RMSE is:", as.numeric(rmseDay)))
  print(paste("Weekly RMSE is:", as.numeric(rmseWeek)))
  print(paste("Monthly RMSE is:", as.numeric(rmseMonth)))
  print(paste("Daily R-Squared is:", as.numeric(rsqDay)))
  print(paste("Weekly R-Squared is:", as.numeric(rsqWeek)))
  print(paste("Monthly R-Squared is:", as.numeric(rsqMonth)))
  print(paste(""))
  return(list(rmseDay = rmseDay, rmseWeek = rmseWeek, rmseMonth = rmseMonth, 
              rsqDay = rsqDay, rsqWeek = rsqWeek, rsqMonth = rsqMonth,
              predDay = pred, predWeek = predWeek, predMonth = predMonth,
              trueDay = data, trueWeek = trueWeek, trueMonth = trueMonth,
              fit = model))
  
  
}

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

mul2014SatGLM <- FutureModelPred(data = mulSatDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
sha2014SatGLM <- FutureModelPred(data = shagSat, providedModel = F, startPredYear = 2014, modelType = "glmnet")
mul2014EstGLM <- FutureModelPred(data = mulPlantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
#shag2014PlantResults <- FutureModelPred(data = shagPlant, providedModel = F, startPredYear = 2014)
cyo2014SatGLM <- FutureModelPred(data = cyohoha$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
sor2014SatGLM <- FutureModelPred(data = sorwathe$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
cyo2014EstGLM <- FutureModelPred(data = cyohoha$plantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
sor2014EstGLM <- FutureModelPred(data = sorwathe$plantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
ruk2014SatGLM <- FutureModelPred(data = rukeri$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
ruk2014EstGLM <- FutureModelPred(data = rukeri$plantDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
#rug2014SatGLM <- FutureModelPred(data = rugundo$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
ass2014SatGLM <- FutureModelPred(data = assopthe$satDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")



saveList = c(ls(pattern = "RF"), ls(pattern = "GLM"))
save(list = saveList, file = "/Users/Tom/Documents/IBI/fullEstateModelsNAisNAnoRF.RData")
#mulCheck <- FutureModelPred(data = mulSatDta, providedModel = T, model = mul2014SatResults$fit, startPredYear = 2014)


estateNames = c("Mulindi", "Shagasha", "Rukeri", "Cyohoha", "Assopthe", "Sorwathe", "Average")
#rowNames = c("Factory Weather Station E-Net", "Factory Weather Station Random Forest", "Satellite Composite E-Net", "Satellite Composite Random Forest")
rowNames = c("Factory Weather Station E-Net", "Satellite Composite E-Net")
df <- data.frame(matrix(ncol = length(estateNames), nrow = length(rowNames)))
colnames(df) <- estateNames
rownames(df) <- rowNames

dayRsq <- weekRsq <- monthRsq <- dayRmse <- weekRmse <- monthRmse <- df

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

dayRsq$Average <- rowMeans(dayRsq[, 1:(ncol(dayRsq-1))], na.rm = T)
weekRsq$Average <- rowMeans(weekRsq[, 1:(ncol(weekRsq-1))], na.rm = T)
monthRsq$Average <- rowMeans(monthRsq[, 1:(ncol(monthRsq-1))], na.rm = T)

dayRsq = round(dayRsq, digits = 2)
weekRsq = round(weekRsq, digits = 2)
monthRsq = round(monthRsq, digits = 2)

write.csv(dayRsq, file = "/Users/Tom/Documents/IBI/rsqResults.csv", row.names = T)
write.csv(weekRsq, file = "/Users/Tom/Documents/IBI/rsqResults.csv", row.names = T)
write.csv(monthRsq, file = "/Users/Tom/Documents/IBI/rsqResults.csv", row.names = T)

# coefDf <- function(model) {
#   coef <- as.matrix(coef(model$finalModel, model$bestTune$lambda)) # select best model coef
#   coef <- round(coef[, 1], 3) # round to 3 digits
#   coef <- coef[order(abs(coef), decreasing = T)] # sort in decreasing order
#   coef <- coef[which(coef != 0)] # cut the zero-variables
#   coef <- coef[!grepl("week", x = names(coef))]
#   coef <- coef[!grepl("month", x = names(coef))]
#   coef <- coef[!grepl("Intercept", x = names(coef))]
#   coefDf <- data.frame(coef)
#   coefDf$names <- names(coef)
#   return(coefDf)
# }

# satList = c(ls(pattern = "SatGLM"))
# for (i in 1:length(satList)) {
#   assign(paste(satList[i], "coef", sep = ""), coefDf(eval(parse(text=paste(satList[i])))$fit))
# }
# 
# timeDropCol <- c("yield", "month", "weekday", "week", "year")
# 
# names(sorwathe$plantDta[ , -which(names(sorwathe$plantDta) %in% timeDropCol)])
#       names(rukeri$plantDta[ , -which(names(rukeri$plantDta) %in% timeDropCol)])
#       names(assopthe$plantDta[ , -which(names(assopthe$plantDta) %in% timeDropCol)])
#       names(cyohoha$plantDta[ , -which(names(cyohoha$plantDta) %in% timeDropCol)])
# 
# corMat <- round(cor(data.frame(cyohoha = cyohoha$plantDta$Cyohoha_Pluv, 
#                            usine = sorwathe$plantDta$Sorwathe_Pluv, 
#                            rukeri = rukeri$plantDta$Rukeri_Pluv), use = "complete.obs"), 
#                            digits = 2)
# 
# corMat2 <- matrix(corMat, ncol = 3, nrow = 3)
# rownames(corMat2) <- names(data.frame(corMat))
# colnames(corMat2) <- names(data.frame(corMat))
# 
# corrplot(corMat, diag = F, type = "upper", method = "number")
# 
# sorLoc <- read.csv("/Users/Tom/Documents/IBI/plantationLoc.csv", stringsAsFactors = F)
# sorLoc <- rename(sorLoc, c("plantationName" = "name"))
# sorLoc[sorLoc$name == "Sorwathe", ]$name <- "Usine"
# sorLoc <- subset(sorLoc, select = c("name", "longitude", "latitude"))
# sorLoc <- rename(sorLoc, replace =  c("longitude" = "lon", "latitude" = "lat"))
# rownames(sorLoc) <- sorLoc$name
# sorLoc <- sorLoc[c( "Cyohoha", "Usine", "Rukeri"), ]
# 
# cyoUsi <- round(distCosine(subset(sorLoc["Cyohoha", ], select = c("lon", "lat")),  
#            subset(sorLoc["Usine", ], select = c("lon", "lat")))/1000, digits = 2)
# cyoRuk <- round(distCosine(subset(sorLoc["Cyohoha", ], select = c("lon", "lat")),  
#             subset(sorLoc["Rukeri", ], select = c("lon", "lat")))/1000, digits = 2)
# rukUsi <- round(distCosine(subset(sorLoc["Rukeri", ], select = c("lon", "lat")),  
#             subset(sorLoc["Usine", ], select = c("lon", "lat")))/1000, digits = 2)
# 
# distMat <- matrix(nrow = 3, ncol = 3)
# rownames(distMat) <- names(data.frame(corMat))
# colnames(distMat) <- names(data.frame(corMat))
# 
# distMat["cyohoha", "cyohoha"] <- 0
# distMat["rukeri", "rukeri"] <- 0
# distMat["usine", "usine"] <- 0
# distMat["cyohoha", "usine"] <- cyoUsi
# distMat["usine", "cyohoha"] <- cyoUsi
# distMat["cyohoha", "rukeri"] <- cyoRuk
# distMat["rukeri", "cyohoha"] <- cyoRuk
# distMat["rukeri", "usine"] <- rukUsi
# distMat["usine", "rukeri"] <- rukUsi
# 
# corrplot(distMat, method = "number", is.corr = F, type = "upper", cl.pos = "n", col = "black", diag = F)
# 
# 
# ### start of index
# 
# mulIncomeTrue <- T
# source('~/Github/IBI/fullMulindiClean.R')
# shagIncomeTrue <- T
# source('~/Github/IBI/shagashaClean.R')
# 
# mul2014SatGLMIncome <- FutureModelPred(data = mulSatDta, providedModel = F, startPredYear = 2014, modelType = "glmnet")
# 
# 
# 
# predYield <- rep(NA, nrow(mulSatDta))
# predYield[complete.cases(mulSatDta)] <- predict(mul2014SatGLMIncome$fit, mulSatDta)
# predYield <- data.frame(predYield = predYield, month = mulSatDta$month, year = mulSatDta$year)
# predYield <- aggregate(predYield ~ month + year, data = predYield, FUN = sum, na.rm = T, na.action = NULL)
# 
# shagIncomeTrue <- T
# source('~/Github/IBI/shagashaClean.R')
# 
# sha2014SatGLMIncome <- FutureModelPred(data = shagSat, providedModel = F, startPredYear = 2014, modelType = "glmnet")
# 
# predYield <- rep(NA, nrow(shagSat))
# predYield[complete.cases(shagSat)] <- predict(sha2014SatGLMIncome$fit, shagSat)
# predYield <- data.frame(predYield = predYield, month = shagSat$month, year = shagSat$year)
# predYield <- aggregate(predYield ~ month + year, data = predYield, FUN = sum, na.rm = T, na.action = NULL)
# #trueMonthIncome <- aggregate(yield ~ as.numeric(as.character(month)) + as.numeric(as.character(year)), data = shagSat, FUN = sum, na.rm = T, na.action = NULL)*100
# 
# 
# #predIncome <- predYield$predYield*100
# trueMonthIncome <- aggregate(yield ~ month + year, data = shagSat, FUN = sum, na.rm = T, na.action = NULL)*100
# 
# # assume 100 RWF per kg 
# # assume max 500
# 
# predIncome <- predYield[4:nrow(predYield),]$predYield*100
# trueMonthIncome <- trueMonthIncome[4:nrow(trueMonthIncome),]
# trueMonthIncome[trueMonthIncome$yield == 0, ]  <- NA
# 
# payoffFloor <- as.numeric(quantile(trueMonthIncome$yield, .15, na.rm = T))
# payoffMax <- 30000
# cost <- payoffMax *.1
# 
# 
# 
# insureIncome <- predIncome
# belowMaxPayoff <- (insureIncome <= (payoffFloor - payoffMax))
# insureIncome[(insureIncome < payoffFloor) & !belowMaxPayoff] <- payoffFloor
# insureIncome[belowMaxPayoff] <- payoffMax
# 
# sum(trueMonthIncome[!(is.na(trueMonthIncome$yield)), "yield"])
# sum(insureIncome[!is.na(trueMonthIncome$yield)])
# #insureIncome[(insureIncome < payoffFloor) & !belowMaxPayoff] <- payoffFloor
# 
# 
# insureIncome <- insureIncome - cost
# 
# ggplot(stack(data.frame(insureIncome = insureIncome, trueIncome = trueMonthIncome$yield)), 
#        aes(x = values)) + geom_density(aes(group = ind, color = ind))
# 
# sd(trueMonthIncome$yield, na.rm = T)/mean(trueMonthIncome$yield, na.rm = T)
# sd(insureIncome, na.rm = T)/mean(insureIncome, na.rm = T)
