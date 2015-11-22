plantationName <- "shagasha"

source("/Users/Tom/Github/IBI/geoFunctions.R")
library(ncdf)
library(data.table)

# set first date and end of files to load

firstYear <- year <- 2010
firstMonth <- month <- 1
firstDay <- day <- 1

endYear <- 2015
endMonth <- 05
endDay <- 31 

merraVars <- c("grn", "rzmc", "prmc", "gwettop", "lai", "tsurf", "sfmc")

for (i in 1:length(merraVars)) {
  # clean MERRA Data
  
  firstYear <- year <- 2010
  firstMonth <- month <- 1
  firstDay <- day <- 1
  
  wd <- "/Users/Tom/Documents/IBI/merra/"
  outWd <- "/Users/Tom/Documents/IBI/shagasha"
  
  # set the desired output variable
  
  merraVariable <- merraVars[i]
  print(merraVariable)
  print(typeof(merraVariable))
  
  # set extent box to downloaded extent 
  # (ncdf doesn't come through into R with file attributes so you have to set them)
  
  box <- extent(28.3333396911621094, 31.6666603088378906, 
                -3.2500000000000000, -0.7500000000000000) 
  
  
  # set points data to pull approriate cells near Shagasha (or other plantation if desired)
  
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
    
    file <- paste(wd, "MERRA300.prod.simul.tavg1_2d_mld_Nx.", year,  month, day, 
                  ".SUB.nc", sep = "")
    
    if(Date >= as.Date("2012-01-01") & Date <= as.Date("2012-03-31")) {
      file <- paste(wd, "MERRA301.prod.simul.tavg1_2d_mld_Nx.", year,  month, day, 
                    ".SUB.nc", sep = "")
    }
    
    rasterName <- paste("merra_", merraVariable, year, "_", month, "_", day, sep = "")
    
    # if file exists, do approriate functions to project raster, adjust it to correct VI ranges
    # crop it to close to Shagasha
    
    if(file.exists(file)) {
      
      nc <- open.ncdf(file) # Open the file
      rasterName <- assign(rasterName, raster(
        get.var.ncdf(nc, merraVariable))) # extract the approraite variable from the file
      close.ncdf(nc) # close it so as to not overload the system
      rasterName <- flip(t(rasterName), direction = "y") # flip the data to correct for mistaken input with ncdf
      crs(rasterName) <- "+proj=longlat +datum=WGS84 +no_defs" # set the crs
      rasterName <- setExtent(rasterName, box) # set the extent as the ncdf doesn't pull the file's extent/crs
      print(file) # print names to check progress
      rasterName <- crop(rasterName, DegreePointExtent(
        subset(points, plantationName == "Shagasha"), .1))  
      
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
  
  merraBrick <- brick(names) # make a brick of the rasters
  merra <- data.frame(date = seq.Date(as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-")), 
                                      as.Date(paste(endYear, as.character(endMonth), as.character(endDay), sep = "-")), by = 1),
                      fileDate = rep(NA, length(names))) 
  for (i in 1:nlayers(merraBrick)) {
    
    # extract the mean evi for the rasters and date (to check correct merging)
    
    merra[i, merraVariable] <- mean(extract(merraBrick[[i]],extent(merraBrick[[i]]) ), na.rm = T)
    merra[i, ]$fileDate <- names(merraBrick[[i]])
  }
  
  write.csv(merra, paste("/Users/Tom/Documents/IBI/shagasha/shagashaMerra_", merraVariable, ".csv", sep = ""), row.names = F)
  
}

wd <- "/Volumes/Tom Passport/chirps/"

Date <- as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-"))

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
  # crop it to close to Shagasha
  
  if(file.exists(file)) {
    rasterName <- assign(rasterName, raster(file))
    print(file) # print names to check progress
    rasterName <- crop(rasterName, DegreePointExtent(
      subset(points, plantationName == "Shagasha"), .1))  
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

write.csv(chirps, "/Users/Tom/Documents/IBI/shagasha/shagashaChirps.csv", row.names = F)

wd <- "/Volumes/Tom Passport/arc/"

Date <- as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-"))

while (as.Date(Date) <= as.Date(paste(endYear, endMonth, endDay, sep = "-"))) { 
  # break apart the day and convert to two characters for reading the files
  
  year <- ConvertToTwoDigits(year(Date))
  month <- ConvertToTwoDigits(month(Date))
  day <- ConvertToTwoDigits(day(Date))
  
  # assign file name and new raster name
  
  
  file <- paste(wd, "africa_arc.", year,  month, day, 
                ".tif", sep = "")
  rasterName <- paste("arc", year, "_", month, "_", day, sep = "")
  
  # if file exists, do approriate functions to project raster, adjust it to correct VI ranges
  # crop it to close to shagasha
  
  if(file.exists(file)) {
    rasterName <- assign(rasterName, raster(file))
    print(file) # print names to check progress
    rasterName <- crop(rasterName, DegreePointExtent(
      subset(points, plantationName == "Shagasha"), .1))
    resampleParam <- rasterName # set resolution for empty raster
  } else { 
    # In case of missing data (which is there is for arc data)
    # set an empty raster with the extent and resolution of the other rasters
    # because it sets the resolution based off the previous raster, this will not 
    # work if the blank raster is the first in the sequence
    
    rasterName <- raster() 
    rasterName <- setValues(rasterName, NA)
    rasterName <- resample(rasterName, resampleParam)
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

arcBrick <- brick(names) # make a brick of the rasters
arc <- data.frame(date = seq.Date(as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-")), 
                                  as.Date(paste(endYear, as.character(endMonth), as.character(endDay), sep = "-")), by = 1),
                  rain = rep(NA, length(names)),
                  fileDate = rep(NA, length(names))) #make an evi  
for (i in 1:nlayers(arcBrick)) {
  
  # extract the mean evi for the rasters and date (to check correct merging)
  
  arc[i, ]$rain <- mean(extract(arcBrick[[i]],extent(arcBrick[[i]]) ), na.rm = T)
  arc[i, ]$fileDate <- names(arcBrick[[i]])
}

write.csv(arc, "/Users/Tom/Documents/IBI/shagasha/shagashaArc.csv", row.names = F)

tamsat <- read.csv("/Users/Tom/Documents/IBI/shagasha/shagasha11kmTAMSAT.csv")
oldNames <- c("Time..YYYYMMDD.", "Mean.rfe", "Standard.deviation", "Valid.values", "Missing.values")
newNames <- c("date", "meanRainfall", "sdRainfall", "validVal", "missVal")
setnames(tamsat, old = oldNames, new = newNames)

keepCol <- c("date", "meanRainfall", "sdRainfall")
tamsat <- subset(tamsat, select = keepCol)
tamsat$date <- as.Date(paste(substr(tamsat$date, 1, 4), substr(tamsat$date, 5, 6), 
                             substr(tamsat$date, 7, 8), sep = "/"), format = "%Y/%m/%d")

write.csv(tamsat, "/Users/Tom/Documents/IBI/shagasha/shagashaTamsat.csv", row.names = F)

wd <- "/Users/Tom/Documents/IBI/evi/"

firstYear <- year <- 2009
firstMonth <- month <- 12
firstDay <- day <- 27

points <- spTransform(points, "+proj=utm +zone=36 +south +datum=WGS84 +units=m 
                      +no_defs +ellps=WGS84 +towgs84=0,0,0")

Date <- as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-"))

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
  # crop it to close to Shagasha
  
  if(file.exists(file)) {
    rasterName <- assign(rasterName, raster(file))
    rasterName <- ViAdjust(rasterName)
    rasterName <- crop(rasterName, 
                       MeterPointExtent(subset(points, plantationName == "Shagasha"), 10000))
    print(file) # print names to check progress
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

write.csv(bimonthEvi, "/Users/Tom/Documents/IBI/shagasha/shagashaBimonthEvi.csv", row.names = F)
