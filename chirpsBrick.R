source("/Users/Tom/Github/IBI/geoFunctions.R")

wd <- "/Volumes/Tom Passport/chirps/"


# set first date and end of files to load

firstYear <- year <- 2010
firstMonth <- month <- 1
firstDay <- day <- 1

endYear <- 2015
endMonth <- 5 #5
endDay <- 31 #31

# set points data to pull approriate cells near Mulindi (or other plantation if desired)


points <- read.csv("/Users/Tom/Documents/IBI/plantationLoc.csv")
points <- subset(points, select = c(plantationName, longitude, latitude))
points <- SpatialPointsDataFrame(subset(points, select = c(longitude, latitude)), points)
crs(points) <- "+proj=longlat +datum=WGS84 +no_defs"

points <- spTransform(points, "+proj=longlat +datum=WGS84 +no_defs")

Date <- as.Date(paste(year, as.character(month), as.character(day), sep = "-"))

while (as.Date(Date) <= as.Date(paste(endYear, endMonth, endDay, sep = "-"))) { 
  # break apart the day and convert to two characters for reading the files
  
  year <- ConvertToTwoDigits(year(Date))
  month <- ConvertToTwoDigits(month(Date))
  day <- ConvertToTwoDigits(day(Date))
  
  # assign file name and new raster name

  file <- paste(wd, "chirps-v2.0.", year, ".",  month, ".", day, 
                ".tif", sep = "")
  rasterName <- paste("chirps", year, "_", month, "_", day, sep = "")
  
  # if file exists, do approriate functions to project raster, adjust it to correct VI ranges
  # crop it to close to Mulindi 
  
  if(file.exists(file)) {
    rasterName <- assign(rasterName, raster(file))
    print(file) # print names to check progress
    rasterName <- crop(rasterName, DegreePointExtent(
      subset(points, plantationName == "Mulindi"), .1))  
  }
  else {
    break
  }
  
  # make a vector of raster Names
  
  if (as.numeric(year) == firstYear & as.numeric(month) == firstMonth & 
      as.numeric(day) == firstDay) {
    names <- rasterName
  } else {
    names <- c(names, rasterName)
  }
  
  Date <- Date + 1 # Move to the next day
}

chirpsBrick <- brick(names) # make a brick of the rasters
chirps <- data.frame(date = seq.Date(as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-")), 
                                     as.Date(paste(endYear, as.character(endMonth), as.character(endDay), sep = "-")), by = 1),
                         rain = rep(NA, length(names)),
                         fileDate = rep(NA, length(names))) #make an evi  
for (i in 1:nlayers(chirpsBrick)) {
  
  # extract the mean evi for the rasters and date (to check correct merging)
  
  chirps[i, ]$rain <- mean(extract(chirpsBrick[[i]],extent(chirpsBrick[[i]]) ), na.rm = T)
  chirps[i, ]$fileDate <- names(chirpsBrick[[i]])
}

write.csv(chirps, "/Users/Tom/Documents/IBI/chirps.csv", row.names = F)