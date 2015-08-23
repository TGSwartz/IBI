# load libraries

library(rgdal)
library(raster)
library(ggplot2)
library(plyr)

# load arbitrary rainfall gauge data

gauge <- read.csv("/Users/Tom/Documents/IBI/rwandaWeatherDataAnalysis/Daily_Max_temperature_Rwanda.csv")

# remove columns unnecessary for mapping

keepCol <- c("station_name", "longitude", "latitude")
gauge <- gauge[, keepCol]
gauge <- rename(gauge, c("station_name" = "name"))

# add type column

gauge$type <- "Rainfall Gauge"

# remove all but one copy of each station
# also remove the one NA row

gauge <- gauge[!duplicated(gauge$name), ]
gauge <- gauge[complete.cases(gauge), ]

# load plantation location data
# a little unsure of the accuracy of the Shagasha dta

plant <- read.csv("/Users/Tom/Documents/IBI/plantationLoc.csv")
plant$type <- "Plantation"
plant <- rename(plant, c("plantationName" = "name"))

# combine data

dta <- rbind(plant, gauge)

# load RWA shapefile

rwaSHP <- readOGR(dsn = "/Users/Tom/Desktop/GIS/IBI/RWA_adm/", layer = "RWA_adm1")
rwaSHP@data$id = rownames(rwaSHP@data)
rwaSHPPoints = fortify(rwaSHP, region = "id")
rwaSHPdf <- join(rwaSHPPoints, rwaSHP@data, by = "id")

#combine

#visPlot <- ggplot() + geom_polygon(data = rwaSHPdf, aes(x = long, y = lat, group = group), fill="#66CCFF", color = "black") + coord_equal() + geom_path(colour = "black") + geom_point(data = gauge, aes(x = longitude, y= latitude), color = "red") + geom_point(data = plant, aes(x = longitude, y = latitude), color = "green") + labs(title = "Rainfall Gauges and Plantation Locations", x = "Longitude", y = "Latitude")


visPlot <- ggplot() + geom_polygon(data = rwaSHPdf, aes(x = long, y = lat, group = group), fill="#66CCFF", color = "black") + coord_equal() + geom_path(colour = "black") + geom_point(data = dta, aes(x = longitude, y= latitude, fill = type, color = type)) + scale_colour_manual(values = c("green", "red")) + labs(title = "Rainfall Gauges and Plantation Locations", x = "Longitude", y = "Latitude")

ggsave(visPlot, file = "/Users/Tom/Documents/IBI/locationPlot.png")

#gauge2 <- spTransform(gauge, CRS("+proj=longlat +datum=WGS84"))
#gauge2 <- fortify(gauge2)

