##########################################
### Functions used in the analysis of  ###
### GIS data and some date formatting  ###
##########################################

library(lubridate)
library(raster)
library(zoo)
library(rgdal)
library(ggplot2)
library(plyr)
library(data.table)
library(maptools)

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


DegreePointExtent <- function(dta, degreeDistance) {
  box <- extent(xmin(dta) - degreeDistance, xmax(dta) + degreeDistance, 
                ymin(dta) - degreeDistance, ymax(dta) + degreeDistance)
}

# creates a bounding box around a point in distance in meters
# input should be a spatial point (or a set of points, which will 
# create a bounding box around all of it)

MeterPointExtent <- function(dta, meterDistance) {
  box <- extent(xmin(dta) - meterDistance, xmax(dta) + meterDistance, 
                ymin(dta) - meterDistance, ymax(dta) + meterDistance)
}

# function that resamples the "value raster" (ndvi, rain, etc.)
# returns a raster with the same resolution and extent of the coverType raster (30cm) 
# the output raster has the extent set and the non-crop cells are made to be NA
# option to set the resample type, defaults to bilinear with nearest neighbor also an option

ValueCropRaster <- function(valueRaster, coverRaster, resampleMethod = "bilinear") {
  valueRaster <- resample(valueRaster, coverRaster, resampleMethod)
  valueRaster <- mask(valueRaster, coverRaster)
}

# create a reprojection parameter file for the MODIS data

ReprojectParam <- function(input, output, paramFile) {
  print(paramFile)
  sink(file = paramFile)
  cat(paste("INPUT_FILENAME = ", "\"", input, "\"", sep = ""))
  cat("\n")
  cat("\n")
  cat("SPECTRAL_SUBSET = (0 1 )")
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
  cat(paste("OUTPUT_FILENAME = ", "\"", output, "\"", sep = ""))
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
  #cat("\n")
  #cat("\n")
  #cat("UTM_ZONE = -35")
  cat("\n") # This last one is important since it won't work properly 
            # without it
  sink()
}

# a function that calculates the correlation between moving averages for different lengths of window
# the input functions are "independent": the variable over which to apply the moving function
# "dependent": the output column, "startLength": the shortest window length, "endLength" the longest window length
# "functionType": the function to apply (mean, sd, etc.)

MovingAverageCorrelation <- function(indepedent, depedent, startLength, endLength, functionType) {
  out <- sapply(startLength:endLength, function(i) cor(rollapplyr(
    indepedent, i, functionType, na.rm = TRUE, fill = NA), depedent, use = "complete.obs"))
  return(out)
}

