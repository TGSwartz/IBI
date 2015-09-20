# load libraries

libraries <- c("raster", "automap", "gstat", "rgdal")
lapply(libraries, require, character.only = TRUE)

# create monthly average

CreateMonthlyAvg <- function(data, startCol, endCol, year, month) {
  data[startCol:endCol] <- apply(data[startCol:endCol], 2, 
                                      function(x) as.numeric(as.character(x))) # change misloaded factors to numerics
  data <- subset(data, Year == year & Month == month)
  data$monAvg <- rowMeans(data[, startCol:endCol], na.rm = T)
  return(data)
}

MakeGISCompatible <- function(data) {
  coordinates(data) <- ~longitude+latitude
  return(data)
}

MonthlyKriging <- function(data, targetCol, shapefile, resolution) {
  
  # create output grid
  grid <- raster(extent(shapefile))
  res(grid) <- resolution
  grid <- rasterToPolygons(grid)
  proj4string(grid) <- proj4string(shapefile)
  grid <- intersect(shapefile, grid)
  
  data$targetColVec <- data[, targetCol]
  data <- MakeGISCompatible(data)
  proj4string(grid) <- proj4string(data)
  
  # krige the target column
  
  out <- autoKrige(formula = targetColVec ~1, input_data = data, new_data = grid)
  return(out)
}

rainGauge <- read.csv("/Users/Tom/Documents/IBI/rwandaWeatherDataAnalysis/Daily_rain_Rwanda.csv")
rwaSHP <- readOGR(dsn = "/Users/Tom/Desktop/GIS/IBI/RWA_adm/", layer = "RWA_adm0")
rainGauge <- CreateMonthlyAvg(rainGauge, 9, 39, 2010, 3)
krigeResult <- MonthlyKriging(rainGauge, "monAvg", rwaSHP, .05)