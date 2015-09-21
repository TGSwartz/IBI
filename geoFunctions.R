# Library of functions for converting rasters to a brick


ConvertToTwoDigits <- function(character) {
  character <- ifelse(as.numeric(character) >= 10 | nchar(character) == 2, as.character(character), paste("0", character, sep = "")) 
}

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