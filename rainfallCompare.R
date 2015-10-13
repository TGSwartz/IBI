# compare rainfall data from TAMSAT, CHIRPS, and Mulindi's Plantation Data

libraries <- c("data.table", "plyr", "TTR", "corrplot", "matrixStats")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

source("/Users/Tom/Github/IBI/bimonthClean.R")

# load the different datasets and keep rainfall estimates, dates (for mergining), and 
# recorded yield for correlation analysis

mulindi <- subset(mulindi, select = c("yield", "date", "rainfall"))
mulindi <- rename(mulindi, c("rainfall" = "plantationRainfall"))

tamsat <- read.csv("/Users/Tom/Documents/IBI/tamsat.csv")
tamsat <- rename(subset(tamsat, select = c("date", "meanRainfall")), c("meanRainfall" = "tamsatRainfall"))
tamsat$date <- as.Date(tamsat$date)
chirps <- read.csv("/Users/Tom/Documents/IBI/chirps.csv")
chirps <- rename(subset(chirps, select = c("date", "rain")), c( "rain" = "chirpsRainfall"))
chirps$date <- as.Date(chirps$date)

# merge the datasets and create median and mean averages

rainDf <- merge(merge(mulindi, tamsat, by = "date", all = T),
                chirps, by = "date", all = F)
avgCol <- c("plantationRainfall", "tamsatRainfall", "chirpsRainfall")
rainDf$medianRainfall <- rowMedians(as.matrix(rainDf[, avgCol]))
rainDf$meanRainfall <- rowMeans(rainDf[, avgCol])

# create a correlation plots (excluding yield)

corCol <- c("plantationRainfall", "tamsatRainfall", "chirpsRainfall", "meanRainfall", "medianRainfall")
customCol <- colorRampPalette(c("red", "grey", "blue"))(100)
jpeg("/Users/Tom/Documents/IBI/rainCorrplot.jpg")
corrplot(cor(rainDf[, corCol], use= "complete.obs"), method = "number", diag = T, 
         type = "upper", tl.srt = 90, col = customCol, tl.col = "black", cl.pos = "n")
dev.off()

#moveAvgDf <- data.frame(plantationRainfall = rep(NA, nrow(rainDf)), 
#                        tamsatSMA = rep(NA, nrow(rainDf)),
#                        chirpsSMA = rep(NA, nrow(rainDf))) 

rainDf <- rainDf[!is.na(rainDf$plantationRainfall), ]

tamsatSMA <- matrix(nrow = nrow(rainDf), ncol = 180)
tamsatCor <- rep(NA, ncol(tamsatSMA))
plantationSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
plantationCor <- rep(NA, ncol(tamsatSMA))
chirpsSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
chirpsCor <- rep(NA, ncol(tamsatSMA))
medianSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
medianCor <- rep(NA, ncol(tamsatSMA))
meanSMA <- matrix(nrow = nrow(rainDf), ncol = ncol(tamsatSMA))
meanCor <- rep(NA, ncol(tamsatSMA))

#tamsamSMA <- apply(rainDf$tamsatRainfall, MARGIN = 1, FUN = function(x) SMA(x, ))

# Find a way to improve with apply

for (i in 1:ncol(tamsatSMA)) {
  tamsatSMA[, i] <- SMA(rainDf$tamsatRainfall, n = i)
  tamsatCor[i] <- cor(tamsatSMA[, i], rainDf$yield, use = "complete.obs")
  plantationSMA[, i] <- SMA(rainDf$plantationRainfall, n = i)
  plantationCor[i] <- cor(plantationSMA[, i], rainDf$yield, use = "complete.obs")
  chirpsSMA[, i] <- SMA(rainDf$chirpsRainfall, n = i)
  chirpsCor[i] <- cor(chirpsSMA[, i], rainDf$yield, use = "complete.obs")
  medianSMA[, i] <- SMA(rainDf$medianRainfall, n = i)
  medianCor[i] <- cor(medianSMA[, i], rainDf$yield, use = "complete.obs")
  meanSMA[, i] <- SMA(rainDf$meanRainfall, n = i)
  meanCor[i] <- cor(meanSMA[, i], rainDf$yield, use = "complete.obs")
}

smaDf <- data.frame(index = seq(1, ncol(tamsatSMA)), 
                    Plantation = plantationCor, TAMSAT = tamsatCor, CHIRPS = chirpsCor,
                    Median = medianCor, Mean = meanCor)

smaPlot <- ggplot(smaDf, aes(index)) + 
  geom_line(aes(y = Plantation, colour = "Plantation")) + 
  geom_line(aes(y = TAMSAT, colour = "TAMSAT")) +
  geom_line(aes(y = CHIRPS, colour = "CHIRPS")) +
  geom_line(aes(y = Median, colour = "Median")) +
  geom_line(aes(y = Mean, colour = "Mean")) +
  ggtitle("Correlation Of Rainfall Estimate Moving Averages with Yield") +
  xlab("Number of Days in Moving Average") + ylab("Correlation Coefficient") +
  scale_colour_discrete(name  ="Rainfall Product", 
                        breaks = c("Plantation", "TAMSAT", "CHIRPS", "Mean", "Median"))
print(smaPlot)  

#print(qplot(seq(1, length(tamsatCor), by = 1), tamsatCor))
#print(qplot(seq(1, length(tamsatCor), by = 1), plantationCor))
#print(qplot(seq(1, length(tamsatCor), by = 1), chirpsCor))