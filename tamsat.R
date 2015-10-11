source('~/Github/IBI/bimonthClean.R')

libraries <- c("data.table", "plyr")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

tamsat <- read.csv("/Users/Tom/Documents/IBI/mulindiDailyTAMSAT11km.csv")
oldNames <- c("Time..YYYYMMDD.", "Mean.rfe", "Standard.deviation", "Valid.values", "Missing.values")
newNames <- c("date", "meanRainfall", "sdRainfall", "validVal", "missVal")
setnames(tamsat, old = oldNames, new = newNames)



keepCol <- c("date", "meanRainfall", "sdRainfall")
tamsat <- subset(tamsat, select = keepCol)
tamsat$date <- as.Date(paste(substr(tamsat$date, 1, 4), substr(tamsat$date, 5, 6), 
                             substr(tamsat$date, 7, 8), sep = "/"), format = "%Y/%m/%d")
