# load libraries

library(rgdal)
library(raster)
library(ggplot2)
library(plyr)
library(data.table)
library(maptools)

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


