source("/Users/Tom/Github/IBI/geoFunctions.R")

wd <- "/Users/Tom/Documents/IBI/evi/"

points <- read.csv("/Users/Tom/Documents/IBI/plantationLoc.csv")
points <- subset(points, select = c(plantationName, longitude, latitude))
points <- SpatialPointsDataFrame(subset(points, select = c(longitude, latitude)), points)
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs"

points <- spTransform(points, "+proj=utm +zone=36 +south +datum=WGS84 +units=m 
                      +no_defs +ellps=WGS84 +towgs84=0,0,0")

# set first date and end of files to load

firstYear <- year <- 2010
firstMonth <- month <- 1
firstDay <- day <- 9

endYear <- 2015
endMonth <- 5 #05
endDay <- 31 #31

Date <- as.Date(paste(year, as.character(month), as.character(day), sep = "-"))

while (as.Date(Date) < as.Date(paste(endYear, endMonth, endDay, sep = "-"))) { 
  # break apart the day and convert to two characters for reading the files
  
  year <- ConvertToTwoDigits(year(Date))
  month <- ConvertToTwoDigits(month(Date))
  day <- ConvertToTwoDigits(day(Date))
  
  # assign file name and new raster name
  
  file <- paste(wd, "MYD13A1_", year, "-",  month, "-", day, 
                ".500m_16_days_EVI.tif", sep = "")
  rasterName <- paste("evi", year, "_", month, "_", day, sep = "")
  
  # if file exists, do approriate functions to project raster, adjust it to correct VI ranges
  # crop it to close to Mulindi 
  
  if(file.exists(file)) {
    rasterName <- assign(rasterName, raster(file))
    #rasterName <- projectRaster(from = assign(rasterName, raster(file)), 
    #                            crs = "+proj=longlat +datum=WGS84 +no_defs")
                                #crs = "+proj=utm +zone=36 + south ellps=WGS84")
    #crs(rasterName) <- "+proj=longlat +datum=WGS84 +no_defs" #"+proj=utm +zone=36 + south ellps=WGS84"
    rasterName <- ViAdjust(rasterName)
    rasterName <- crop(rasterName, 
                       MeterPointExtent(subset(points, plantationName == "Mulindi"), 15000))
    #rasterName <- crop(rasterName, DegreePointExtent("/Users/Tom/Documents/IBI/plantationLoc.csv", 
    #                                               "plantationName", "Mulindi", .05))  
  }
  else {
    break
  }
  print(file) # print names to check progress
  # make a vector of raster Names
  
  if (as.numeric(year) == firstYear & as.numeric(month) == firstMonth & 
      as.numeric(day) == firstDay) {
    names <- rasterName
  } else {
    names <- c(names, rasterName)
  }
  
  Date <- Date + 16 # Move to the next 16 day composite
  
  # this resets the day to the 9th when the loop rolls over to a new year
  
  if (year(Date) > year) {
    Date <- as.Date(paste(year(Date), month(Date), "09", sep = "-"))
  }
}

bimonthBrick <- brick(names) # make a brick of the rasters
bimonthEvi <- data.frame(bimonthID = seq(from = 1, to = length(names), by  = 1),
                         evi = rep(NA, length(names)),
                         fileDate = rep(NA, length(names))) #make an evi  
for (i in 1:nlayers(bimonthBrick)) {
  
  # extract the mean evi for the rasters and date (to check correct merging)
  
  bimonthEvi[i, ]$evi <- mean(extract(bimonthBrick[[i]],extent(bimonthBrick[[i]]) ), na.rm = T)
  bimonthEvi[i, ]$fileDate <- names(bimonthBrick[[i]])
}

write.csv(bimonthEvi, "/Users/Tom/Documents/IBI/mulindiBimonthEvi.csv", row.names = F)