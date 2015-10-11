# compare rainfall data from TAMSAT, CHIRPS, and Mulindi's Plantation Data

libraries <- c("data.table", "plyr")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

source("/Users/Tom/Github/IBI/bimonthClean.R")

mulindi <- subset(mulindi, select = c("date", "yield", "rainfall"))
mulindi <- rename(mulindi, c("rainfall" = "plantationRainfall"))

tamsat <- read.csv("/Users/Tom/Documents/IBI/tamsat.csv")
tamsat <- rename(subset(tamsat, select = c("date", "meanRainfall")), c("meanRainfall" = "tamsatRainfall"))
tamsat$date <- as.Date(tamsat$date)
chirps <- read.csv("/Users/Tom/Documents/IBI/chirps.csv")
chirps <- rename(subset(chirps, select = c("date", "rain")), c( "rain" = "chirpsRainfall"))
chirps$date <- as.Date(chirps$date)

rainDf <- merge(merge(mulindi, tamsat, by = "date", all = T),
                chirps, by = "date", all = F)
avgCol <- c("plantationRainfall", "tamsatRainfall", "chirpsRainfall")
rainDf$medianRainfall <- rowMedians(as.matrix(rainDf[, avgCol]))
rainDf$meanRainfall <- rowMeans(rainDf[, avgCol])

corCol <- c("yield", "plantationRainfall", "tamsatRainfall", "chirpsRainfall", "meanRainfall", "medianRainfall")
jpeg("/Users/Tom/Documents/IBI/rainCorrplot.jpg")
corrplot(cor(rainDf[, corCol], use= "complete.obs"), method = "number", diag = F, 
         type = "upper", tl.srt = 90, col = customCol, tl.col = "black", cl.pos = "n")
dev.off()