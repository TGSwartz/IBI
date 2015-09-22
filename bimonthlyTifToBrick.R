source("/Users/Tom/Github/IBI/geoFunctions.R")


ConvertToTwoDigits <- function(character) {
  character <- ifelse(as.numeric(character) >= 10 | nchar(character) == 2, as.character(character), paste("0", character, sep = "")) 
}

wd <- "/Users/Tom/Documents/IBI/ndvi/"

year <- 2010
month <- 1

# searches for the first day that has a composite, then breaks the for loop 
# and returns the day of the month

for (firstDay in 1:16) {
  month <- ConvertToTwoDigits(month)
  firstDay <- ConvertToTwoDigits(firstDay)
  file <- paste(wd, "MYD13A1_", year, "-",  month, "-", firstDay, 
                ".500m_16_days_EVI.tif", sep = "")
  if (file.exists(file)) {
    break
  }
}


Date <- as.Date(paste(year, month, as.character(firstDay), sep = "-"))
while (as.Date(Date) < Sys.Date()) {
  
  Date <- Date + 16
}
