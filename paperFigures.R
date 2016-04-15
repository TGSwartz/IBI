#####################################
#### Create final paper figures #####
#####################################

source('~/Github/IBI/fullMulindiClean.R')
source("/Users/Tom/Github/IBI/geoFunctions.R")

libraries <- c("data.table", "plyr", "TTR", "matrixStats", "corrplot", "zoo", "ggplot2")
lapply(libraries, require, character.only = TRUE)

outputWd <- "/Users/Tom/Documents/IBI/"

################################
#### Figure 6           #####
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
#### Figure 15              ####
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
  #geom_line(aes(y = tMaxCor, colour = "tMaxCor")) + 
  #geom_line(aes(y = tMinCor, colour = "tMinCor")) + 
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
#### Figures 8 & 9          ####
#### Demonstrations of EVI  ####
#### and CHIRPS data        ####
################################

# load Rwanda shapefile

rwaSHP <- readOGR(dsn = "/Users/Tom/Documents/IBI/RWA_adm/", layer = "RWA_adm0")

# load demonstration EVI file, adjust it to the correct units and crs, then crop and mask

eviRaster <- raster("/Volumes/Tom Passport/evi/MYD13A1_2010-01-09.500m_16_days_EVI.tif")
eviRaster <- ViAdjust(eviRaster)
eviRaster <- projectRaster(eviRaster, crs = "+proj=longlat +datum=WGS84 +no_defs")
eviRaster <- crop(eviRaster, extent(rwaSHP))
eviRaster <- mask(eviRaster, rwaSHP)

jpeg(paste(outputWd, "eviDemonstration.jpg", sep = ""))
plot(eviRaster)
dev.off()

chirpsRaster <- raster("/Volumes/Tom Passport/chirps/chirps-v2.0.2010.01.09.tif")
chirpsRaster <- crop(chirpsRaster, extent(rwaSHP))
chirpsRaster <- mask(chirpsRaster, rwaSHP)

jpeg(paste(outputWd, "chirpsDemonstration.jpg", sep = ""))
plot(chirpsRaster)
dev.off()

################################
#### Figure 2: Locations    ####
#### of Estates and         ####
#### RMA weather stations   ####
################################


# load arbitrary rainfall gauge data

gauge <- read.csv("/Users/Tom/Documents/IBI/rwandaWeatherDataAnalysis/Daily_Max_temperature_Rwanda.csv")

# remove columns unnecessary for mapping

keepCol <- c("station_name", "longitude", "latitude")
gauge <- gauge[, keepCol]
gauge <- rename(gauge, c("station_name" = "name"))

# add type column

gauge$type <- "RMA Rainfall Gauge"

# remove all but one copy of each station
# also remove the one NA row

gauge <- gauge[!duplicated(gauge$name), ]
gauge <- gauge[complete.cases(gauge), ]

# removes a station that is recorded as south of Rwanda's border.  
# worth checking out for later. In particular if it implies other locations might be off.

gauge <- gauge[gauge$latitude != min(gauge$latitude), ] 

# load plantation location data
# a little unsure of the accuracy of the Shagasha dta

plant <- read.csv("/Users/Tom/Documents/IBI/plantationLoc.csv")
plant$type <- as.character(plant$plantationName)
plant[plant$type == "Sorwathe", ]$type <- "Usine"
plant <- rename(plant, c("plantationName" = "name"))
plant <- subset(plant, select = c("name", "longitude", "latitude", "type"))

# combine data

dta <- rbind(plant, gauge)

# load RWA shapefile

rwaSHP <- readOGR(dsn = "/Users/Tom/Documents/IBI/RWA_adm/", layer = "RWA_adm1")
rwaSHP@data$id = rownames(rwaSHP@data)
rwaSHPPoints = fortify(rwaSHP, region = "id")
rwaSHPdf <- join(rwaSHPPoints, rwaSHP@data, by = "id")

#combine

visPlot <- ggplot() + geom_polygon(data = rwaSHPdf, aes(x = long, y = lat, group = group),
                                   fill="white", color = "black") + coord_equal() + 
  #fill="#66CCFF", color = "black") + coord_equal() + 
  geom_path(colour = "black") + 
  geom_point(data = dta, aes(x = longitude, y= latitude, fill = type, color = type)) + 
  scale_colour_manual(values = c("green", "red", "yellow", "navy", "orange",  "pink", "purple")) + 
  labs(title = "Rainfall Gauges and Plantation Locations", x = "Longitude", y = "Latitude") 
#scale_fill_discrete(breaks = c("Assopthe", "Cyohoha", "Mulindi", "Rukeri", "Shagasha", "Usine", "RMA Rainfall Gauge"))

ggsave(visPlot, file = "/Users/Tom/Documents/IBI/locationPlot.png")
