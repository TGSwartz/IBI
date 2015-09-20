# source ModisDownload file and set wd properly so the correct RData is also available

setwd("/Users/Tom/Documents/IBI/ndvi")
source('~/Desktop/GIS/IBI/ModisDownload.R')

product <- 55 # set modis product to 50, which is Aqua Monthly 1km tiles
# MYD13A1 is 55 which is aqua 16 day 500m
horiz <- c(20, 21)
vert <- c(9) # also set horizontal and vertical tiles to the total of Rwanda
dates <- c("2011.01.01,", "2011.12.31") # set dates to all of 2011
band <- "0 1 0 0 0 0 0 0 0 0 0" # set band to 1, which is Enhanced Vegetation Index (EVI)

# set data directory for MRT

Sys.setenv(MRT_DATA_DIR  = "/Users/Tom/Desktop/MRT/data")

# Download

Download <- function(dates) {
  ModisDownload(x = product ,h = horiz,v = vert,dates = dates,
                MRTpath="/Users/Tom/Desktop/MRT/bin", bands_subset = band, mosaic = T,proj = T,
                proj_type = "UTM",utm_zone = 30,datum = "WGS84", pixel_size = 463.3127)
}
  
Reproject <- function(dates) {
  mosaic()
}



