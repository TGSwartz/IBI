libraries <- c("data.table", "xts", "lubridate", "TTR", "plyr", "caTools")
lapply(libraries, require, character.only = TRUE, quietly = TRUE)

firstYear <- 2010
firstMonth <- 1
firstDay <- 9

# read data and reset names and convert to time
MulindiClean <- function(){
  mulindi <- read.csv("/Users/Tom/Documents/IBI/MULINDIdata.csv")
  
  oldNames <- c("DATE", "RANFAIL", "T.MAX", "T.MIN", "PRODUCTION")
  newNames <- c("date", "rainfall", "tMax", "tMin", "yield")
  setnames(mulindi, old = oldNames, new = newNames)
  
  mulindi$date <- as.Date(mulindi$date, "%d/%m/%Y")
  
  # some of the years are coded in as 2 digit and some as 4.  
  # So this next line makes years under 2000 become four digit (ex: 10 -> 2010)
  
  year(mulindi$date) <- ifelse(mulindi$date < 2000, 2000 + 
                                 year(mulindi$date), year(mulindi$date))
  
  # remove NA rows that are blanks between months in the csv
  
  mulindi <- mulindi[!is.na(mulindi$date), ]
  
  # make NAs in yield and rainfall 0s
  # check to make sure this is ok
  
  #mulindi[is.na(mulindi$rainfall),]$rainfall <- 0
  #mulindi[is.na(mulindi$yield),]$yield <- 0
  
  # remove probable anomolous tMin
  
  #mulindi <- mulindi[mulindi$tMin > 7, ]
  
  #mulindi <- mulindi[!is.na(mulindi$tMax), ] # removes
  mulindi$month <- as.factor(month(mulindi$date)) # factor month for regression purposes
  #mulindi$day <- as.factor(day(mulindi$date))
  #mulindi$year <- as.factor(as.numeric(year(mulindi$date))-2010) # convert year to 0 for 2010 as a base
  mulindi$year <- as.factor(year(mulindi$date))
  return(mulindi)
}

mulindi <- MulindiClean()
#source("/Users/Tom/Github/IBI/biMonthlyTifToBrick()")



mulindi <- mulindi[mulindi$date 
                   >= as.Date(paste(firstYear, firstMonth, as.character(firstDay), 
                                    sep = "-")), ] # cut days before first day where there's no MODIS day

date <- as.Date(paste(firstYear, firstMonth, as.character(firstDay), sep = "-")) # make date a date object

# this creates a bimonth ID that cycles up every 16 days or to Jan 9 when a new year

count <- 1
internalCount <- 1
bimonthID <- rep(NA, nrow(mulindi))

for(i in 1:nrow(mulindi)) {
  if(month(date) == 1 & day(date) == 1) {
    internalCount <- 9
    mulindi$bimonthID[i] <- count
  }
  if(internalCount == 17) {
    internalCount <- 1
    count <- count + 1
  }
  mulindi$bimonthID[i] <- count
  internalCount <- internalCount + 1
  date <- date + 1
}

#mulindi <- merge(tamsat, mulindi, by  = "date", all.x = F, all.y = F)
#mulindi$rainfall <- mulindi$meanRainfall
mulindi <- mulindi[mulindi$tMin > 7, ] # remove anomalous observations
#mulindi <- aggregate(. ~ bimonthID, mulindi, FUN = mean)
#mulindi$month <- as.factor(floor(mulindi$month)) # make months a factor and round them down (abritary, could be rounded up but needs to be rounded)
bimonthEvi <- read.csv("/Users/Tom/Documents/IBI/mulindiBimonthEvi.csv", stringsAsFactors = F)
mulindi <- merge(mulindi, bimonthEvi)
mulindi <- mulindi[order(mulindi$date), ]
mulindi <- mulindi[,!(names(mulindi) %in% c("fileDate"))]
#mulindi <- mulindi[,!(names(mulindi) %in% c("date"))]
mulindi$tMinAvg <- rollapply(mulindi$tMin, width = which.max(tMinCor), mean, na.rm = T, fill = NA, align = "right") # average of 4 periods determined by correlation
mulindi$rainfallAvg <- rollapply(rainDf$medianRainfall, width = which.max(medianCor), mean, na.rm = T, fill = NA, align = "right") # average of 4 periods determined by correlation# average of 4 periods determined by correlation
mulindi$tMaxAvg <- rollapply(mulindi$tMax, width = which.max(tMaxCor), mean, na.rm = T, fill = NA, align = "right") # average of 4 periods determined by correlation # average of 2 periods determined by correlation
mulindi$time <- seq(1, nrow(mulindi), by = 1)

write.csv(mulindi, "/Users/Tom/Documents/IBI/mulindiDisaggData.csv")

# mulindi <- aggregate(. ~ bimonthID, mulindi, FUN = mean)
# mulindi$tMinAvg <- SMA(mulindi$tMin, 4) # average of 4 periods determined by correlation
# mulindi$rainfallAvg <- SMA(mulindi$rainfall, 4) # average of 4 periods determined by correlation
# mulindi$tMaxAvg <- SMA(mulindi$tMax, 2) # average of 2 periods determined by correlation
# mulindi$year <- floor(mulindi$year + 2009) 
# mulindi <- mulindi[,!(names(mulindi) %in% c("date"))]
# mulindi$month <- as.factor(floor(mulindi$month)) # make months a factor and round them down (abritary, could be rounded up but needs to be rounded)
# write.csv(mulindi, "/Users/Tom/Documents/IBI/mulindiAggData.csv")

trainCol <- c("yield", #"rainfall", "tMax", "tMin", 
              "month", "evi","tMinAvg", 
             "rainfallAvg", "tMaxAvg", "time", "year")
mulindiTrainDf <- subset(mulindi, select = trainCol)



