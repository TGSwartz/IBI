# load libraries

libraries <- c("data.table", "xts", "lubridate", "TTR", "plyr")
lapply(libraries, require, character.only = TRUE)

# read data and reset names and convert to time
mulindi <- read.csv("/Users/Tom/Documents/IBI/MULINDIdata.csv")

oldNames <- c("DATE", "RANFAIL", "T.MAX", "T.MIN", "PRODUCTION")
newNames <- c("date", "rainfall", "tMax", "tMin", "yield")
setnames(mulindi, old = oldNames, new = newNames)

mulindi$date <- as.Date(mulindi$date, "%d/%m/%Y")

# some of the years are coded in as 2 digit and some as 4.  
# So this next line makes years under 2000 become four digit (ex: 10 -> 2010)

year(mulindi$date) <- ifelse(mulindi$date < 2000, 2000 + year(mulindi$date), year(mulindi$date))

# remove NA rows that are blanks between months in the csv

mulindi <- mulindi[!is.na(mulindi$date), ]

# make NAs in yield and rainfall 0s
# check to make sure this is ok

mulindi[is.na(mulindi$rainfall),]$rainfall <- 0
mulindi[is.na(mulindi$yield),]$yield <- 0

# remove probable anomolous tMin

mulindi <- mulindi[mulindi$tMin > 7, ]

# convert to xts object


mulindi <- mulindi[!is.na(mulindi$tMax), ]
mulindi$month <- as.factor(month(mulindi$date))
#mulindi$day <- as.factor(day(mulindi$date))
mulindi$year <- as.factor(as.numeric(year(mulindi$date))-2010)

# moving averages

#mulindi$rainAvg <- SMA(mulindi$rainfall, n = 30)
#mulindi$tMinAvg <- SMA(mulindi$tMin, n = 60)
GDDval <- (mulindi$tMax+mulindi$tMin)/2 - 10
movsum <- function(x,n=90){filter(x,rep(1,n), sides=1)}
mulindi$GDDSum <- movsum(GDDval)
mulindi$rainSum <- movsum(mulindi$rainfall)
mulindi$tMinSum <- movsum(mulindi$tMin)

# yearly sum of GDD

# mulindi$year <- ifelse(as.numeric(as.character(mulindi$month)) <= 8, as.numeric(as.character(mulindi$month)), as.numeric(as.character(mulindi$month)) + 1)
# GDDSumDf <- ddply(mulindi, .(year), summarize, cumsum(GDDval))
# names(GDDSumDf)[2] <- "GDDSum"
# mulindi$GDDSum <- GDDSumDf$GDDSum
# rm(GDDSumDf)
# mulindi$year <- as.numeric(year(mulindi$date))-2010

mulindiXts <- xts(subset(mulindi, select = -c(date)), order.by = mulindi$date)

Analysis2011 <- function(mulindi) {
  #mulindi <- mulindi[mulindi$year == 1, ] # year 1 is 2011
  dta <- subset(mulindi, select = -c(date, GDDSum, rainSum, tMinSum))
  dta <- aggregate(. ~ month + year, dta, FUN = mean)
}

