#####################################
#### Create final paper figures #####
#####################################

source('~/Github/IBI/fullMulindiClean.R')
source("/Users/Tom/Github/IBI/geoFunctions.R")

libraries <- c("data.table", "plyr", "TTR", "matrixStats", "corrplot", "zoo", "ggplot2")
lapply(libraries, require, character.only = TRUE)

outputWd <- "/Users/Tom/Documents/IBI/"

################################
#### Figure XXXX           #####
#### Rainfall comparision  #####
################################

# create a correlation plots comparing different rainfall products

corCol <- c("plantationRainfall", "tamsatRainfall", "chirpsRainfall", "arcRainfall", "meanRainfall", "medianRainfall")
newNames <- c("Estate Rainfall", "TAMSAT Rainfall", "CHIRPS rainfall", "ARC Rainfall", "Mean Sat Rainfall", "Median Sat Rainfall")
corColDf <- setnames(rainDf[, corCol], old = corCol, new = newNames)
customCol <- colorRampPalette(c("red", "grey", "blue"))(100)
jpeg(paste(outputWd, "rainCorrplot.jpg", sep = ""))
corrplot(cor(corColDf, use= "complete.obs"), method = "number", diag = T, 
         type = "lower", tl.srt = 90, col = customCol, tl.col = "black", cl.pos = "n")
dev.off()

################################
#### Figure XXXX            ####
#### Plot of moving average ####
#### correlations           ####
################################


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

corDf <- data.frame(index = seq(1, 100), 
                    Plantation = plantationCor, TAMSAT = tamsatCor, CHIRPS = chirpsCor, ARC = arcCor,
                    Median = medianCor, Mean = meanCor, medianSD = medianSD)

corPlot <- ggplot(corDf, aes(index)) + 
  geom_line(aes(y = Plantation, colour = "Plantation")) + 
  geom_line(aes(y = TAMSAT, colour = "TAMSAT")) +
  geom_line(aes(y = CHIRPS, colour = "CHIRPS")) +
  geom_line(aes(y = ARC, colour = "ARC")) +
  geom_line(aes(y = Median, colour = "Satellite Median")) +
  geom_line(aes(y = Mean, colour = "Satellite Mean")) +
  geom_line(aes(y = medianSD, colour = "Satellite Median SD")) + 
  ggtitle("Correlation Of Rainfall Estimate Moving Averages with Yield") +
  xlab("Number of Days in Moving Average") + ylab("Correlation Coefficient") +
  scale_colour_discrete(name  ="Rainfall Product", 
                        breaks = c("Plantation", "TAMSAT", "CHIRPS", "ARC", 
                                   "Satellite Mean", "Satellite Median", 
                                   "Satellite Median SD"))
jpeg(paste(outputWd, "yieldCorPlot.jpg", sep = ""))
print(corPlot)  
dev.off()

################################
#### Figures XXXX & YYYY    ####
#### Demonstrations of EVI  ####
#### and CHIRPS data        ####
################################

# load Rwanda shapefile

rwaSHP <- readOGR(dsn = "/Users/Tom/Desktop/GIS/IBI/RWA_adm/", layer = "RWA_adm0")

# load demonstration EVI file, adjust it to the correct units and crs, then crop and mask

eviRaster <- raster("/Users/Tom/Documents/IBI/evi/MYD13A1_2010-01-09.500m_16_days_EVI.tif")
eviRaster <- ViAdjust(eviRaster)
eviRaster <- projectRaster(eviRaster, crs = "+proj=longlat +datum=WGS84 +no_defs")
eviRaster <- crop(eviRaster, extent(rwaSHP))
eviRaster <- mask(eviRaster, rwaSHP)

jpeg(paste(outputWd, "eviDemonstration.jpg", sep = ""))
plot(eviRaster)
dev.off()

chirpsRaster <- raster("/Users/Tom/Documents/IBI/chirps/chirps-v2.0.2010.01.09.tif")
chirpsRaster <- crop(chirpsRaster, extent(rwaSHP))
chirpsRaster <- mask(chirpsRaster, rwaSHP)

jpeg(paste(outputWd, "chirpsDemonstration.jpg", sep = ""))
plot(chirpsRaster)
dev.off()
