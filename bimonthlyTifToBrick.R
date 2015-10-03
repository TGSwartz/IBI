source("/Users/Tom/Github/IBI/geoFunctions.R")

wd <- "/Users/Tom/Documents/IBI/evi/out/"

# set first date and end of files to load

firstYear <- year <- 2010
firstMonth <- month <- 1
firstDay <- day <- 9

endYear <- 2015
endMonth <- 05
endDay <- 31

Date <- as.Date(paste(year, month, as.character(day), sep = "-"))

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
    rasterName <- projectRaster(from = assign(rasterName, raster(file)), 
                                crs = "+proj=longlat +datum=WGS84 +no_defs")
    rasterName <- ViAdjust(rasterName) 
    rasterName <- crop(rasterName, MakePointExtent("/Users/Tom/Documents/IBI/plantationLoc.csv", 
                                                   "plantationName", "Mulindi", .025))  
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
#bimonthEvi <- rep(NA, length(names)) # create an empty vector for the evi
bimonthEvi <- data.frame(bimonthID = seq(from = 1, to = length(names), by  = 1),
                         evi = rep(NA, length(names)),
                         date = rep(NA, length(names))) #make an evi  
for (i in 1:length(names)) {
  # extract the mean evi for the rasters and date (to check correct merging)
  bimonthEvi[i, ]$evi <- mean(extract(bimonthBrick[[i]],extent(bimonthBrick[[i]]) ), na.rm = T)
  bimonthEvi[i, ]$date <- names(bimonthBrick[[i]])
}

write.csv(bimonthEvi, "/Users/Tom/Documents/IBI/mulindiBimonthEvi.csv", row.names = F)