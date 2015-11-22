source("/Users/Tom/Github/IBI/geoFunctions.R")

library(ncdf)

wd <- "/Users/Tom/Documents/IBI/merra/"
  #"/Volumes/Tom Passport/merra/"

merraVars <- c("grn", "rzmc", "prmc", "gwettop", "lai", "tsurf", "sfmc")

for (i in 1:length(merraVars)) {

# set the desired output variable

  merraVariable <- merraVars[i]
  
  # set extent box to downloaded extent 
  # (ncdf doesn't come through into R with file attributes so you have to set them)
  
  box <- extent(28.3333396911621094, 31.6666603088378906, 
                -3.2500000000000000, -0.7500000000000000) 
  
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
    
    file <- paste(wd, "MERRA300.prod.simul.tavg1_2d_mld_Nx.", year,  month, day, 
                  ".SUB.nc", sep = "")
    
    if(Date >= as.Date("2012-01-01") & Date <= as.Date("2012-03-31")) {
      file <- paste(wd, "MERRA301.prod.simul.tavg1_2d_mld_Nx.", year,  month, day, 
                    ".SUB.nc", sep = "")
    }
    
    rasterName <- paste("merra_", merraVariable, year, "_", month, "_", day, sep = "")
    
    # if file exists, do approriate functions to project raster, adjust it to correct VI ranges
    # crop it to close to Mulindi 
    
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
  
  merraBrick <- brick(names) # make a brick of the rasters
  merra <- data.frame(date = seq.Date(as.Date(paste(firstYear, as.character(firstMonth), as.character(firstDay), sep = "-")), 
                                      as.Date(paste(endYear, as.character(endMonth), as.character(endDay), sep = "-")), by = 1),
                      fileDate = rep(NA, length(names))) 
  for (i in 1:nlayers(merraBrick)) {
    
    # extract the mean evi for the rasters and date (to check correct merging)
    
    merra[i, merraVariable] <- mean(extract(merraBrick[[i]],extent(merraBrick[[i]]) ), na.rm = T)
    merra[i, ]$fileDate <- names(merraBrick[[i]])
  }
  
  write.csv(merra, paste("/Users/Tom/Documents/IBI/mulindiMerra_", merraVariable, ".csv", sep = ""), row.names = F)
  
}



# rast <- raster(get.var.ncdf(open.ncdf("/Users/Tom/Documents/IBI/merra/MERRA300.prod.simul.tavg1_2d_mld_Nx.20100101.SUB.nc"), "sfmc"))
# rastTrans <- flip(t(rast), direction = "y")
# crs(rastTrans) <- "+proj=longlat +datum=WGS84 +no_defs"
# rastTrans <- setExtent(rastTrans, box)