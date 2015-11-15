##########################################
### Clean and assemble the datasets to ###
### form the final Mulindi data frame  ###
##########################################

# load libraries and functions

source('~/Github/IBI/geoFunctions.R')

libraries <- c("data.table", "xts", "lubridate", "TTR", "plyr", "caTools", "matrixStats")
lapply(libraries, require, character.only = TRUE)

# set the first date of analysis

firstYear <- 2010
firstMonth <- 1
firstDay <- 1

# read data and reset names and convert to time

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


# factor month and year for regression purposes and create a weekday object
# create a time (cumulative days) variable

mulindi$month <- as.factor(month(mulindi$date))
mulindi$year <- as.factor(year(mulindi$date))
mulindi$weekday <- weekdays(as.Date(mulindi$date))
mulindi$time <- seq(1, nrow(mulindi), by = 1)

# make date a date object

date <- as.Date(paste(firstYear, firstMonth, as.character(firstDay), sep = "-")) 

# create a bimonth ID for the bimonthly MODIS data. This will allow merging with the MODIS data
# this creates a bimonth ID that cycles up every 16 days or 
# to Jan 9 for a new year (Jan 9 is the automatic reset date for each new year)

# the count represents the bimonthly ID while the internal code cycles through
# 16 times and then resets and increases the count variable

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

# load the bimonth EVI and merge with the mulindi dataframe
# reorder the dataset by date and remove the fileDate column from the bimonth ID (included for checking purposes)

bimonthEvi <- read.csv("/Users/Tom/Documents/IBI/mulindiBimonthEvi.csv", stringsAsFactors = F)
mulindi <- merge(mulindi, bimonthEvi, by = "bimonthID")
mulindi <- mulindi[order(mulindi$date), ]
mulindi <- mulindi[,!(names(mulindi) %in% c("fileDate"))]

wd <- "/Users/Tom/Documents/IBI/"

# load the different merra products

merraGrn <- read.csv(paste(wd, "merra_grn.csv", sep = ""))
merraRzmc <- read.csv(paste(wd, "merra_rzmc.csv", sep = ""))
merraPrmc <- read.csv(paste(wd, "merra_prmc.csv", sep = ""))
merraGwettop <- read.csv(paste(wd, "merra_gwettop.csv", sep = ""))
merraLai <- read.csv(paste(wd, "merra_lai.csv", sep = ""))
merraTsurf <- read.csv(paste(wd, "merra_tsurf.csv", sep = ""))
merraSfmc <- read.csv(paste(wd, "merra_sfmc.csv", sep = ""))

# merge all the files to a dataframe for analysis
# suppress warnings that the file names are duplicated (irrelevant)

suppressWarnings(merra <- merge(merge(merge(merge(merge(merge(merraGrn, merraRzmc, by = "date"), merraPrmc, by = "date"),
                                                  merraGwettop, by = "date"), merraLai, by = "date"), merraTsurf, by = "date"),
                                merraSfmc, by = "date"))

# keep the good data and get rid of the file names

keepCol <- c("date", "grn", "rzmc", "prmc", "gwettop", "lai", "tsurf", "sfmc")

# surface temp in K, convert to C

merra$tsurf <- merra$tsurf -273 

# keep the good columns

merra <- subset(merra, select = keepCol) 

# change date type for merging

merra$date <- as.Date(merra$date) #
merra <- merge(mulindi, merra, by = "date")

# create moving average variables for MERRA at point of maximum correlation with yield

mulindi$merraRzmc <- merra$rzmc
mulindi$merraRzmcAvg <- rollapply(merra$rzmc, width = which.max(MovingAverageCorrelation(merra$rzmc, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindi$merraPrmc <- merra$prmc
mulindi$merraPrmcAvg <- rollapply(merra$prmc, width = which.max(MovingAverageCorrelation(merra$prmc, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindi$merraSfmc <- merra$sfmc
mulindi$merraSfmcAvg <- rollapply(merra$sfmc, width = which.max(MovingAverageCorrelation(merra$sfmc, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindi$merraGrn <- merra$grn
mulindi$merraGrnAvg <- rollapply(merra$grn, width = which.max(MovingAverageCorrelation(merra$grn, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindi$merraLai <- merra$lai
mulindi$merraLaiAvg <- rollapply(merra$lai, width = which.max(MovingAverageCorrelation(merra$lai, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindi$merraTsurf <- merra$tsurf
mulindi$merraTsurfAvg <- rollapply(merra$tsurf, width = which.max(MovingAverageCorrelation(merra$tsurf, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindi$merraGwettop <- merra$gwettop
mulindi$merraGwettopAvg <- rollapply(merra$gwettop, width = which.max(MovingAverageCorrelation(merra$gwettop, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")


# load the different datasets and keep rainfall estimates, dates (for merging), and 
# recorded yield for correlation analysis

mulindi <- rename(mulindi, c("rainfall" = "plantationRainfall"))

tamsat <- read.csv("/Users/Tom/Documents/IBI/tamsat.csv")
tamsat <- rename(subset(tamsat, select = c("date", "meanRainfall")), c("meanRainfall" = "tamsatRainfall"))
tamsat$date <- as.Date(tamsat$date)

chirps <- read.csv("/Users/Tom/Documents/IBI/chirps.csv")
chirps <- rename(subset(chirps, select = c("date", "rain")), c( "rain" = "chirpsRainfall"))
chirps$date <- as.Date(chirps$date)

arc <- read.csv("/Users/Tom/Documents/IBI/arc.csv")
arc <- rename(subset(arc, select = c("date", "rain")), c("rain" = "arcRainfall"))
arc$date <- as.Date(arc$date)

# merge the datasets and create median and mean averages
# only include satellite products for averaging to avoid NAs in the plantation data (and as a test of satellite only quality)

rainDf <- merge(merge(merge(mulindi, tamsat, by = "date", all.x = T, all.y = F),
                      chirps, by = "date", all.x = T, all.y = F), arc, by = "date", all.x = T, all.y = F)

avgCol <- c("tamsatRainfall", "chirpsRainfall", "arcRainfall")
rainDf$medianRainfall <- rowMedians(as.matrix(rainDf[, avgCol]))
rainDf$meanRainfall <- rowMeans(rainDf[, avgCol])

# create moving average variables at point of maximum correlation

mulindi$tMinAvg <- rollapply(mulindi$tMin, width = which.max(MovingAverageCorrelation(mulindi$tMin, mulindi$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right") 
mulindi$tMaxAvg <- rollapply(mulindi$tMax, width = which.max(MovingAverageCorrelation(mulindi$tMax, mulindi$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right") 

mulindi$medianRainfall <- rainDf$medianRainfall
mulindi$medianRainfallAvg <- rollapply(rainDf$medianRainfall, width = which.max(MovingAverageCorrelation(rainDf$medianRainfall, mulindi$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right") 
mulindi$medianRainfallSD <- rollapply(rainDf$medianRainfall, width = which.max(MovingAverageCorrelation(rainDf$medianRainfall, mulindi$yield, 2, 100, sd)), sd, na.rm = T, fill = NA, align = "right")

# rearrange columns

mulindi <- rename(mulindi, replace = c("time" = "cumulativeDays", "medianRainfall" = "medianSatRainfall", "medianRainfallAvg" = "medianSatRainfallAvg", "medianRainfallSD"  = "medianSatRainfallSD"))
colOrder <- c("cumulativeDays", "date", "year", "month", "weekday", "yield", "tMax", "tMaxAvg", "tMin", "tMinAvg", "plantationRainfall",
              "evi",  "medianSatRainfall", "medianSatRainfallAvg", "medianSatRainfallSD", "merraRzmc", "merraRzmcAvg", "merraPrmc", "merraPrmcAvg", "merraSfmc", "merraSfmcAvg",
              "merraGrn", "merraGrnAvg", "merraLai", "merraLaiAvg", "merraTsurf", #"merraTsurfAvg", 
              "merraGwettop",
              "merraGwettopAvg")
mulindi <- mulindi[colOrder]

# write csv for mulindi data

write.csv(mulindi, "/Users/Tom/Documents/IBI/mulindiFullData.csv", row.names = F)

# aggregate and write a csv for data aggregated at the monthly level

mulindiMth <- aggregate(. ~ month + year, data = subset(mulindi, select = -c(weekday, date, cumulativeDays)), FUN = mean, na.action = na.omit )
#mulindiMth <- mulindiMth[with(mulindiMth, order(year, month)), ]
write.csv(mulindiMth, "/Users/Tom/Documents/IBI/mulindiMonthAgg.csv", row.names = F)

# seperate out variables that are available with using the plantation data only

plantCol <- c("yield", "date", "year", "month", "weekday", "tMax", "tMaxAvg", "tMin", "tMinAvg", "plantationRainfall")
plantDta <- subset(mulindi, select = plantCol)

# drop the plantation data variables for satellite only analysis (keeping yield of course)

satDropCol <- c("tMax", "tMaxAvg", "tMin", "tMinAvg", "plantationRainfall")
satDta <- subset(mulindi, select = -c(tMax, tMaxAvg, tMin, tMinAvg, plantationRainfall))
