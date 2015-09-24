source("/Users/Tom/Github/IBI/geoFunctions.R")

library(lubridate)

wd <- "/Users/Tom/Documents/IBI/evi/"

year <- 2010
month <- 1
day <- 9

# searches for the first day that has a composite, then breaks the for loop 
# and returns the day of the month

# THIS HAS BEEN REMOVED AS FIRST DAY IS ALWAYS THE 9TH

# for (firstDay in 1:16) {
#   month <- ConvertToTwoDigits(month)
#   firstDay <- ConvertToTwoDigits(firstDay)
#   file <- paste(wd, "MYD13A1_", year, "-",  month, "-", firstDay, 
#                 ".500m_16_days_EVI.tif", sep = "")
#   if (file.exists(file)) {
#     break
#   }
# }




Date <- as.Date(paste(year, month, as.character(firstDay), sep = "-"))

while (as.Date(Date) < Sys.Date()) {
  print(Date)
  
  # break apart the day and convert to two characters for reading the files
  
  year <- ConvertToTwoDigits(year(Date))
  month <- ConvertToTwoDigits(month(Date))
  day <- ConvertToTwoDigits(day(Date))
  file <- paste(wd, "MYD13A1_", year, "-",  month, "-", day, 
                ".500m_16_days_EVI.tif", sep = "")
  if(file.exists(file)) {
    print(file)
  }
  Date <- Date + 16 # Move to the next 16 day composite
  
  # this resets the day to the 9th when the loop rolls over to a new year
  
  if (year(Date) > year) {
    Date <- as.Date(paste(year(Date), month(Date), "09", sep = "-"))
  }
}
