library(lubridate)
library(raster)

# Library of functions for converting rasters to a brick
# doesn't work yet

QuiettLoadLib <- function(libraries) {
  lapply(libraries, require, character.only = TRUE, quietly = TRUE)
}

# converts a input (numeric or character) to a 2 day digit character for reading in file names

ConvertToTwoDigits <- function(character) {
  character <- ifelse(as.numeric(character) >= 10 | nchar(character) == 2, as.character(character), paste("0", character, sep = "")) 
}

# makes the adjustments necessary for vegetation index

ViAdjust <- function(data) {
  data[data == -3000] <- NA # -3000 is the NA value for MODIS
  data <- data *.0001 # data is stored multipled by 10000
}

# takes a point's lat and lon and makes a bounding box around it up to a distance in degrees
# specified by the user

MakePointExtent <- function(pointFile, rowName, point, degreeDistance) {
  csv <- read.csv(pointFile)
  lon <- csv[csv[, rowName] == point, ]$longitude
  lat <- csv[csv[, rowName] == point, ]$latitude
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

ValueCropRaster <- function(valueRaster, coverRaster, resampleMethod = "bilinear") {
  valueRaster <- resample(valueRaster, coverRaster, resampleMethod)
  valueRaster <- mask(valueRaster, coverRaster)
}

ReprojectParam <- function(input, output, paramFile) {
  print(paramFile)
  sink(file = paramFile)
  cat(paste("INPUT_FILENAME = ", input, sep = ""))
  cat("\n")
  cat("\n")
  cat("SPECTRAL_SUBSET = ( 1 )")
  cat("\n")
  cat("\n")
  cat("SPATIAL_SUBSET_TYPE = INPUT_LAT_LONG")
  cat("\n")
  cat("\n")
  cat("SPATIAL_SUBSET_UL_CORNER = ( 0.0 19.999999998 )")
  cat("\n")
  cat("SPATIAL_SUBSET_LR_CORNER = ( -9.999999999 40.617064472 )")
  cat("\n")
  cat("\n")
  cat(paste("OUTPUT_FILENAME = ", output, sep = ""))
  cat("\n")
  cat("\n")
  cat("RESAMPLING_TYPE = NEAREST_NEIGHBOR")
  cat("\n")
  cat("\n")
  cat("OUTPUT_PROJECTION_TYPE = UTM")
  cat("\n")
  cat("\n")
  cat("OUTPUT_PROJECTION_PARAMETERS = ( 
       0.0 0.0 0.0
       0.0 0.0 0.0
       0.0 0.0 0.0
       0.0 0.0 0.0
       0.0 0.0 0.0 )
       ")
  cat("\n")
  cat("\n")
  cat("DATUM = WGS84")
  cat("\n")
  cat("\n")
  cat("UTM_ZONE = 0")
  cat("\n") # This last one is important since it won't work properly 
            # without it
  sink()
}
