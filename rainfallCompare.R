# compare rainfall data from TAMSAT, CHIRPS, and Mulindi's Plantation Data

libraries <- c("data.table", "plyr", "TTR", "matrixStats", "corrplot", "zoo", "ggplot2")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

#source("/Users/Tom/Github/IBI/bimonthClean.R")
source('~/Github/IBI/merraCorTest.R')

# load the different datasets and keep rainfall estimates, dates (for mergining), and 
# recorded yield for correlation analysis

#mulindi <- subset(mulindi, select = c("yield", "date", "rainfall", "tMax", "tMin", "evi"))
mulindiTrainDf <- rename(mulindiTrainDf, c("rainfall" = "plantationRainfall"))

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

rainDf <- merge(merge(merge(mulindiTrainDf, tamsat, by = "date", all.x = T, all.y = F),
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


tamsatCor <- MovingAverageCorrelation(rainDf$tamsatRainfall, rainDf$yield, 1, 100, "mean")
plantationCor <- MovingAverageCorrelation(rainDf$plantationRainfall, rainDf$yield, 1, 100, "mean")
chirpsCor <- MovingAverageCorrelation(rainDf$chirpsRainfall, rainDf$yield, 1, 100, "mean")
arcCor <- MovingAverageCorrelation(rainDf$arcRainfall, rainDf$yield, 1, 100, "mean")
medianCor <- MovingAverageCorrelation(rainDf$medianRainfall, rainDf$yield, 1, 100, "mean")
meanCor <- MovingAverageCorrelation(rainDf$meanRainfall, rainDf$yield, 1, 100, "mean")
tMaxCor <- MovingAverageCorrelation(rainDf$tMax, rainDf$yield, 1, 100, "mean")
tMinCor <- MovingAverageCorrelation(rainDf$tMin, rainDf$yield, 1, 100, "mean")
eviCor <- MovingAverageCorrelation(rainDf$evi, rainDf$yield, 1, 100, "mean")
medianSD <- MovingAverageCorrelation(rainDf$medianRainfall, rainDf$yield, 2, 100, "sd")
medianSD[100] <- medianSD[99] # since the Standard Deviation only works for a minimum of 2, make the 100th the same as 99 to match the vector lengths for plotting purposes

smaDf <- data.frame(index = seq(1, 100), 
                    Plantation = plantationCor, TAMSAT = tamsatCor, CHIRPS = chirpsCor, ARC = arcCor,
                    Median = medianCor, Mean = meanCor, medianSD = medianSD)

smaPlot <- ggplot(smaDf, aes(index)) + 
  geom_line(aes(y = Plantation, colour = "Plantation")) + 
  geom_line(aes(y = TAMSAT, colour = "TAMSAT")) +
  geom_line(aes(y = CHIRPS, colour = "CHIRPS")) +
  geom_line(aes(y = ARC, colour = "ARC")) +
  geom_line(aes(y = Median, colour = "Median")) +
  geom_line(aes(y = Mean, colour = "Mean")) +
  geom_line(aes(y = medianSD, colour = "Median SD")) + 
  ggtitle("Correlation Of Rainfall Estimate Moving Averages with Yield") +
  xlab("Number of Days in Moving Average") + ylab("Correlation Coefficient") +
  scale_colour_discrete(name  ="Rainfall Product", 
                        breaks = c("Plantation", "TAMSAT", "CHIRPS", "ARC", "Mean", "Median", "Median SD"))
print(smaPlot)  

mulindiTrainDf$tMinAvg <- rollapply(mulindi$tMin, width = which.max(tMinCor), mean, na.rm = T, fill = NA, align = "right") # average of 4 periods determined by correlation
mulindiTrainDf$medianRainfallAvg <- rollapply(rainDf$medianRainfall, width = which.max(medianCor), mean, na.rm = T, fill = NA, align = "right") # average of 4 periods determined by correlation# average of 4 periods determined by correlation
mulindiTrainDf$tMaxAvg <- rollapply(mulindi$tMax, width = which.max(tMaxCor), mean, na.rm = T, fill = NA, align = "right") # average of 4 periods determined by correlation # average of 2 periods determined by correlation
mulindiTrainDf$medianRainfall <- rainDf$medianRainfall
mulindiTrainDf$medianRainfallSD <- rollapply(rainDf$medianRainfall, width = which.max(medianSD), sd, na.rm = T, fill = NA, align = "right")


#mulindiTrainDf <- subset(mulindiTrainDf, select=-c(plantationRainfall))

# rearrange columns

mulindiTrainDf <- rename(mulindiTrainDf, replace = c("time" = "cumulativeDays", "medianRainfall" = "medianSatRainfall", "medianRainfallAvg" = "medianSatRainfallAvg", "medianRainfallSD"  = "medianSatRainfallSD"))
colOrder <- c("cumulativeDays", "date", "year", "month", "weekday", "yield", "tMax", "tMaxAvg", "tMin", "tMinAvg", "plantationRainfall",
              "evi",  "medianSatRainfall", "medianSatRainfallAvg", "medianSatRainfallSD", "merraRzmc", "merraRzmcAvg", "merraPrmc", "merraPrmcAvg", "merraSfmc", "merraSfmcAvg",
              "merraGrn", "merraGrnAvg", "merraLai", "merraLaiAvg", "merraTsurf", #"merraTsurfAvg", 
              "merraGwettop",
              "merraGwettopAvg")
mulindiTrainDf <- mulindiTrainDf[colOrder]

write.csv(mulindiTrainDf, "/Users/Tom/Documents/IBI/mulindiFullData.csv", row.names = F)

mulindiTrainDfMth <- aggregate(. ~ month + year, data = subset(mulindiTrainDf, select = -c(weekday, date, cumulativeDays)), FUN = mean, na.action = na.omit )
mulindiTrainDfMth <- mulindiTrainDfMth[with(mulindiTrainDfMth, order(year, month)), ]
write.csv(mulindiTrainDfMth, "/Users/Tom/Documents/IBI/mulindiMonthAgg.csv", row.names = F)

plantCol <- c("yield", "date", "year", "month", "weekday", "tMax", "tMaxAvg", "tMin", "tMinAvg", "plantationRainfall")
plantDta <- subset(mulindiTrainDf, select = plantCol)

satDropCol <- c("tMax", "tMaxAvg", "tMin", "tMinAvg", "plantationRainfall")
satDta <- subset(mulindiTrainDf, select = -c(tMax, tMaxAvg, tMin, tMinAvg, plantationRainfall))
