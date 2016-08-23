inWd <- "/Volumes/Tom Passport/hdf/preMulindi/"
outWd <- "/Users/Tom/Desktop/modis/"
source('~/Github/IBI/geoFunctions.R')

#sink("/Users/Tom/Documents/IBI/MRTparameters.prm")
#input <- file

firstYear <- year <- 2002
firstMonth <- month <- 07
firstDay <- day <- 4

Date <- as.Date(paste(year, month, as.character(day), sep = "-"))
i <- 1

# while (as.Date(Date) < as.Date(paste(2010, 01, 10, sep = "-"))) { #as.Date(paste(2011, 1, 20, sep = "-"))) {  
#   # break apart the day and convert to two characters for reading the files
#   
#   year <- ConvertToTwoDigits(year(Date))
#   month <- ConvertToTwoDigits(month(Date))
#   day <- ConvertToTwoDigits(day(Date))
#   
#   input <- paste(inWd, "Mosaic_", year, "-",  month, "-", day, 
#                 ".hdf", sep = "")
#   output <- paste(outWd, "MYD13A1_", year, "-", month, "-", day, ".tif", sep = "")
#                   #".500m_16_days_EVI.tif"
#   paramFile <- paste("/Users/Tom/Desktop/MRT/param", i, ".prm", sep = "")
#   ReprojectParam(input, output, paramFile)
#   
#   # Then go to console and type the following commands
#   # cd "/Users/Tom/Desktop/MRT" or approriate MRT directory
#   # resample -p param1.prm, etc. (or use a command to resample them all)
#   
# 
#   print(input)
#   if(file.exists(input) == F) {
#     break
#   }
# 
#   Date <- Date + 16 # Move to the next 16 day composite
#   
#   # this resets the day to the 9th when the loop rolls over to a new year
#   
#   if (year(Date) > year) {
#     Date <- as.Date(paste(year(Date), month(Date), "09", sep = "-"))
#   }
#   i <- i + 1
# }

# dateList <- matrix(nrow = 1, ncol = 173)
# 
# while (as.Date(Date) < as.Date(paste(2010, 01, 1, sep = "-"))) { #as.Date(paste(2011, 1, 20, sep = "-"))) {  
#   # break apart the day and convert to two characters for reading the files
#   
#   year <- ConvertToTwoDigits(year(Date))
#   month <- ConvertToTwoDigits(month(Date))
#   day <- ConvertToTwoDigits(day(Date))
#   jul <- as.numeric(julian(Date, origin = as.Date(paste(year(Date), "01", "01", sep = "-"), format = "%Y-%m-%d")))+1
#   
#   #input <- paste(inWd, "Mosaic_", year, "-",  month, "-", day, 
#   #               ".hdf", sep = "")
#   #output <- paste(outWd, "MYD13A1_", year, "-", month, "-", day, ".tif", sep = "")
#   #".500m_16_days_EVI.tif"
#   #input1 <- paste(inWd, "MYD13A1.A", year, jul, "h20v.hdf")
#   #input2 <- paste(inWd, "MYD13A1.A", year, jul, "h21v.hdf")
#   
#   jul <- as.character(jul)
#   jul <- ifelse(nchar(jul) == 1, 
#                         paste("00", jul, sep = ""),
#                         as.character(jul))
#   jul <- ifelse(nchar(jul) == 2, 
#                 paste("0", jul, sep = ""),
#                 as.character(jul)) 
#   if (length(jul) == 1) {
#     
#   }
#   
#   date <- paste(year, jul, sep = "")
#   
#   # Then go to console and type the following commands
#   # cd "/Users/Tom/Desktop/MRT" or approriate MRT directory
#   # resample -p param1.prm, etc. (or use a command to resample them all)
#   
#   
#   print(date)
#   dateList[1, i] <- date
#   #if(file.exists(input) == F) {
#   #  break
#   #}
#   
#   Date <- Date + 16 # Move to the next 16 day composite
#   
#   # this resets the day to the 9th when the loop rolls over to a new year
#   
#   if (year(Date) > year) {
#     Date <- as.Date(paste(year(Date), month(Date), "09", sep = "-"))
#   }
#   i <- i + 1
# }


while (as.Date(Date) < as.Date(paste(2010, 01, 1, sep = "-"))) { #as.Date(paste(2011, 1, 20, sep = "-"))) {  
  # break apart the day and convert to two characters for reading the files
  
  year <- ConvertToTwoDigits(year(Date))
  month <- ConvertToTwoDigits(month(Date))
  day <- ConvertToTwoDigits(day(Date))
  
  jul <- as.numeric(julian(Date, origin = as.Date(paste(year(Date), "01", "01", sep = "-"), format = "%Y-%m-%d")))+1
  
  jul <- as.character(jul)
  jul <- ifelse(nchar(jul) == 1, 
                paste("00", jul, sep = ""),
                as.character(jul))
  jul <- ifelse(nchar(jul) == 2, 
                paste("0", jul, sep = ""),
                as.character(jul)) 
  if (length(jul) == 1) {
    
  }
  
  input <- paste(inWd, "mosaic_", year, jul, 
                ".hdf", sep = "")
  output <- paste(outWd, "MYD13A1_", year, "-", month, "-", day, ".tif", sep = "")
                  #".500m_16_days_EVI.tif"
  paramFile <- paste("/Users/Tom/Desktop/MRT/param", i, ".prm", sep = "")
  ReprojectParam(input, output, paramFile)
  
  # Then go to console and type the following commands
  # cd "/Users/Tom/Desktop/MRT" or approriate MRT directory
  # resample -p param1.prm, etc. (or use a command to resample them all)
  

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
