# compare rainfall data from TAMSAT, CHIRPS, and Mulindi's Plantation Data

libraries <- c("data.table", "plyr", "TTR", "matrixStats", "corrplot", "zoo", "ggplot2")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

source("/Users/Tom/Github/IBI/bimonthClean.R")

# load the different datasets and keep rainfall estimates, dates (for mergining), and 
# recorded yield for correlation analysis

mulindi <- subset(mulindi, select = c("yield", "date", "rainfall", "tMax", "tMin", "evi"))
mulindi <- rename(mulindi, c("rainfall" = "plantationRainfall"))

tamsat <- read.csv("/Users/Tom/Documents/IBI/tamsat.csv")
tamsat <- rename(subset(tamsat, select = c("date", "meanRainfall")), c("meanRainfall" = "tamsatRainfall"))
tamsat$date <- as.Date(tamsat$date)
chirps <- read.csv("/Users/Tom/Documents/IBI/chirps.csv")
chirps <- rename(subset(chirps, select = c("date", "rain")), c( "rain" = "chirpsRainfall"))
chirps$date <- as.Date(chirps$date)
arc <- read.csv("/Users/Tom/Documents/IBI/arc.csv")
arc <- rename(subset(arc, select = c("date", "rain")), c("rain" = "arcRainfall"))
arc$date <- as.Date(arc$date)

# merge the datasets and create median and mean averages

rainDf <- merge(merge(merge(mulindi, tamsat, by = "date", all.x = T, all.y = F),
                chirps, by = "date", all.x = T, all.y = F), arc, by = "date", all.x = T, all.y = F)
#avgCol <- c("plantationRainfall", "tamsatRainfall", "chirpsRainfall", "arcRainfall")
avgCol <- c("tamsatRainfall", "chirpsRainfall", "arcRainfall")
rainDf$medianRainfall <- rowMedians(as.matrix(rainDf[, avgCol]))
rainDf$meanRainfall <- rowMeans(rainDf[, avgCol])

# create a correlation plots (excluding yield)

corCol <- c("plantationRainfall", "tamsatRainfall", "chirpsRainfall", "arcRainfall", "meanRainfall", "medianRainfall")
customCol <- colorRampPalette(c("red", "grey", "blue"))(100)
jpeg("/Users/Tom/Documents/IBI/rainCorrplot.jpg")
corrplot(cor(rainDf[, corCol], use= "complete.obs"), method = "number", diag = T, 
         type = "upper", tl.srt = 90, col = customCol, tl.col = "black", cl.pos = "n")
dev.off()

#moveAvgDf <- data.frame(plantationRainfall = rep(NA, nrow(rainDf)), 
#                        tamsatSMA = rep(NA, nrow(rainDf)),
#                        chirpsSMA = rep(NA, nrow(rainDf))) 

#rainDf <- rainDf[!is.na(rainDf$plantationRainfall), ]

tamsatSMA <- matrix(nrow = nrow(rainDf), ncol = 100)
tamsatCor <- rep(NA, ncol(tamsatSMA))
plantationSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
plantationCor <- rep(NA, ncol(tamsatSMA))
chirpsSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
chirpsCor <- rep(NA, ncol(tamsatSMA))
medianSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
medianCor <- rep(NA, ncol(tamsatSMA))
meanSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
meanCor <- rep(NA, ncol(tamsatSMA))
tMaxSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
tMaxCor <- rep(NA, ncol(tamsatSMA))
tMinSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
tMinCor <- rep(NA, ncol(tamsatSMA))
arcSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
arcCor <- rep(NA, ncol(tamsatSMA))
eviSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
eviCor <- rep(NA, ncol(tamsatSMA))

#tamsamSMA <- apply(rainDf$tamsatRainfall, MARGIN = 1, FUN = function(x) SMA(x, ))

# Find a way to improve with apply

for (i in 1:ncol(tamsatSMA)) {
  tamsatSMA[, i] <- rollapply(rainDf$tamsatRainfall, width = i, mean, na.rm = T, fill = NA, align = "right")
  tamsatCor[i] <- cor(tamsatSMA[, i], rainDf$yield, use = "complete.obs")
  plantationSMA[, i] <- rollapply(rainDf$plantationRainfall, width = i, mean, na.rm = T, fill = NA, align = "right")
  plantationCor[i] <- cor(plantationSMA[, i], rainDf$yield, use = "complete.obs")
  chirpsSMA[, i] <- rollapply(rainDf$chirpsRainfall, width = i, mean, na.rm = T, fill = NA, align = "right")
  chirpsCor[i] <- cor(chirpsSMA[, i], rainDf$yield, use = "complete.obs")
  arcSMA[, i] <- rollapply(rainDf$arcRainfall, width = i, mean, na.rm = T, fill = NA, align = "right")
  arcCor[i] <- cor(arcSMA[, i], rainDf$yield, use = "complete.obs")
  medianSMA[, i] <- rollapply(rainDf$medianRainfall, width = i, mean, na.rm = T, fill = NA, align = "right")
  medianCor[i] <- cor(medianSMA[, i], rainDf$yield, use = "complete.obs")
  meanSMA[, i] <- rollapply(rainDf$meanRainfall, width = i, mean, na.rm = T, fill = NA, align = "right")
  meanCor[i] <- cor(meanSMA[, i], rainDf$yield, use = "complete.obs")
  tMaxSMA[, i] <- rollapply(rainDf$tMax, width = i, mean, na.rm = T, fill = NA, align = "right")
  tMaxCor[i] <- cor(tMaxSMA[, i], rainDf$yield, use = "complete.obs")
  tMinSMA[, i] <- rollapply(rainDf$tMin, width = i, mean, na.rm = T, fill = NA, align = "right")
  tMinCor[i] <- cor(tMinSMA[, i], rainDf$yield, use = "complete.obs")
  eviSMA[, i] <- rollapply(rainDf$evi, width = i, mean, na.rm = T, fill = NA, align = "right")
  eviCor[i] <- cor(eviSMA[, i], rainDf$yield, use = "complete.obs")
}

smaDf <- data.frame(index = seq(1, ncol(tamsatSMA)), 
                    Plantation = plantationCor, TAMSAT = tamsatCor, CHIRPS = chirpsCor, ARC = arcCor,
                    Median = medianCor, Mean = meanCor)

smaPlot <- ggplot(smaDf, aes(index)) + 
  geom_line(aes(y = Plantation, colour = "Plantation")) + 
  geom_line(aes(y = TAMSAT, colour = "TAMSAT")) +
  geom_line(aes(y = CHIRPS, colour = "CHIRPS")) +
  geom_line(aes(y = ARC, colour = "ARC")) +
  geom_line(aes(y = Median, colour = "Median")) +
  geom_line(aes(y = Mean, colour = "Mean")) +
  ggtitle("Correlation Of Rainfall Estimate Moving Averages with Yield") +
  xlab("Number of Days in Moving Average") + ylab("Correlation Coefficient") +
  scale_colour_discrete(name  ="Rainfall Product", 
                        breaks = c("Plantation", "TAMSAT", "CHIRPS", "ARC", "Mean", "Median"))
print(smaPlot)  

#print(qplot(seq(1, length(tamsatCor), by = 1), tamsatCor))
#print(qplot(seq(1, length(tamsatCor), by = 1), plantationCor))
#print(qplot(seq(1, length(tamsatCor), by = 1), chirpsCor))