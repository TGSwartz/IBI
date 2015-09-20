library(raster)
library(rgdal)

# constants

numRaster <- 68 

# convert a block of tif rasters to a raster brick

wd <- "/Users/Tom/Documents/IBI/ndvi/"
rwaSHP <- readOGR(dsn = "/Users/Tom/Documents/IBI/RWA_adm/", layer = "RWA_adm0")

NdviAdjust <- function(data) {
  data[data == 0] <- NA
  data <- data *.0001
  data <- crop(data, extent(rwaSHP))
}

MakePointExtent <- function(pointFile, rowName, point, degreeDistance) {
  csv <- read.csv(pointFile)
  lon <- csv[csv$plantationName == "Mulindi", ]$longitude
  lat <- csv[csv$plantationName == "Mulindi", ]$latitude
  upperLon <- lon + degreeDistance
  lowerLon <- lon - degreeDistance
  upperLat <- lat + degreeDistance
  lowerLat <- lat - degreeDistance
  box <- extent(lowerLon, upperLon, lowerLat, upperLat) # vector (length=4; order= xmin, xmax, ymin, ymax)
  return(box)
}

# function that resamples the "value raster" (ndvi, rain, etc.)
# returns a raster with the same resolution and extent of the coverType raster (30cm) 
# the output raster has the extent set and the non-crop cells are made to be NA
# option to set the resample type, defaults to bilinear with nearest neighbor also an option

ValueCropRaster <- function(valueRaster, coverRaster, resample = "bilinear") {
  valueRaster <- resample(valueRaster, coverRaster, resample)
#   coverRaster[coverRaster != 3] <- NA # sets all non cropland cells to NA
#   coverRaster[coverRaster == 3] <- 1 # sets all cropland cells to 1 for multiplication
  valueRaster <- valueRaster * coverRaster
}

coverType <- projectRaster(
  raster("/Users/Tom/Documents/IBI/servir-rwanda_landcover_2010_scheme_i.tif"), 
  crs = "+proj=longlat +datum=WGS84 +no_defs")

# cropland = 3

coverType <- crop(coverType, MakePointExtent("/Users/Tom/Documents/IBI/plantationLoc.csv", 
                                             plantationName, Mulindi, .025))
coverType[coverType != 3] <- NA # sets all non cropland cells to NA
coverType[coverType == 3] <- 1 # sets all cropland cells to 1 for multiplication

for (j in 2010:2015) {
  for (i in 1:12) {
    i <- ifelse(i >= 10, as.character(i), paste("0", i, sep = "")) 
    file <-  paste(wd, "MYD13A3_", j, "-", i, "-01.1_km_monthly_EVI.tif", sep = "")
    print(file)
    rasterName <- paste("evi", j, "_", i, sep = "")
    print(rasterName)
    if(file.exists(paste(wd, "MYD13A3_", j, "-", i, "-01.1_km_monthly_EVI.tif", sep = ""))) {
      rasterName <- projectRaster(from = assign(rasterName, raster(file)), 
                                  crs = "+proj=longlat +datum=WGS84 +no_defs")
      rasterName <- NdviAdjust(rasterName)
      rasterName <- crop(rasterName, MakePointExtent("/Users/Tom/Documents/IBI/plantationLoc.csv", 
                                                     plantationName, Mulindi, .025))  
      rasterName <- ValueCropRaster(rasterName, coverType)
      
      if (as.numeric(i) == 1 & as.numeric(j) == 2010) {
        names <- rasterName
      }
      else {
        names <- c(names, rasterName)
      }
    }
    
  }
} 


brick <- brick(names)

ndvi <- rep(NA, numRaster)

for (i in 1:numRaster) {
  ndvi[i] <- mean(extract(brick[[i]],extent(brick[[i]]) ), na.rm = T)
}



