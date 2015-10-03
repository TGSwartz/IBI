inWd <- "/Users/Tom/Documents/IBI/evi/hdf/16Day/"
outWd <- "/Users/Tom/Documents/IBI/evi/out/"
source('~/Github/IBI/geoFunctions.R')

#sink("/Users/Tom/Documents/IBI/MRTparameters.prm")
#input <- file

firstYear <- year <- 2010
firstMonth <- month <- 1
firstDay <- day <- 9

Date <- as.Date(paste(year, month, as.character(day), sep = "-"))
i <- 1

while (as.Date(Date) < as.Date(paste(2015, 07, 20, sep = "-"))) { #as.Date(paste(2011, 1, 20, sep = "-"))) {  
  # break apart the day and convert to two characters for reading the files
  
  year <- ConvertToTwoDigits(year(Date))
  month <- ConvertToTwoDigits(month(Date))
  day <- ConvertToTwoDigits(day(Date))
  
  input <- paste(inWd, "Mosaic_", year, "-",  month, "-", day, 
                ".hdf", sep = "")
  output <- paste(outWd, "MYD13A1_", year, "-", month, "-", day, ".tif", sep = "")
                  #".500m_16_days_EVI.tif"
  paramFile <- paste("/Users/Tom/Desktop/MRT/param", i, ".prm", sep = "")
  ReprojectParam(input, output, paramFile)
  
  # Then go to console and type the following commands
  # cd "/Users/Tom/Desktop/MRT" or approriate MRT directory
  # 
  

  print(input)
  if(file.exists(input) == F) {
    break
  }

  Date <- Date + 16 # Move to the next 16 day composite
  
  # this resets the day to the 9th when the loop rolls over to a new year
  
  if (year(Date) > year) {
    Date <- as.Date(paste(year(Date), month(Date), "09", sep = "-"))
  }
  i <- i + 1
}