# load appriorate functions

source('~/Github/IBI/geoFunctions.R')

libraries <- c("data.table", "xts", "lubridate", "TTR", "plyr", "caTools", "matrixStats")
lapply(libraries, require, character.only = TRUE)

# set first date

firstYear <- 2010
firstMonth <- 1
firstDay <- 1

# load data and clean names

shagasha <- read.csv("/Users/Tom/Documents/IBI/shagasha/shagashaProductivity.csv", 
                     skip = 5, stringsAsFactors = F, na.strings = "")

oldNames <- c("DATE", "RAINFALL", "PRODUCTION")
newNames <- c("date", "plantationRainfall", "yield")
setnames(shagasha, old = oldNames, new = newNames)

# many blank columns are spuriously loaded so only keep the real ones

shagasha <- subset(shagasha, select = newNames) 

# remove rows that have no dates (which are sums of rainfall and production for that month)

shagasha <- shagasha[!is.na(shagasha$date), ]

# change date to date type
# merge with an empty dataframe of all dates for the shagasha period (there are missing dates in the shagasha file)
# create factors for month, year, weekday, and weeks

shagasha$date <- as.Date(shagasha$date, format = "%m/%d/%y")
shagasha <- merge(shagasha, data.frame(date = 
                                         seq.Date(shagasha[1, ]$date, 
                                                  shagasha[nrow(shagasha), ]$date, 
                                                  by = 1)), all = T)

shagasha$month <- as.factor(month(shagasha$date))
shagasha$year <- as.factor(year(shagasha$date))
shagasha$weekday <- as.factor(weekdays(as.Date(shagasha$date)))
shagasha$week <- as.factor(week(shagasha$date))

# replace non-numeric valeus as zero

shagasha[!is.na(shagasha$plantationRainfall) & shagasha$plantationRainfall == "NT", ]$plantationRainfall <- NA
shagasha[!is.na(shagasha$yield) & shagasha$yield == " -   ", ]$yield <- NA

# match end date to mulindi

shagasha <- shagasha[shagasha$date <= "2015-05-31", ]

# change strings to numerics and remove commas in csv file

shagasha$plantationRainfall <- as.numeric(shagasha$plantationRainfall)
suppressWarnings(shagasha$yield <- as.numeric(gsub(",", "", shagasha$yield)))

# scale and center the yield estimates

shagasha$yield <- as.numeric(scale(shagasha$yield, center = T, scale = T))

# create evi bimonthly ID for merging
# then merge with shagasha EVI file

count <- 1
internalCount <- 1
bimonthID <- rep(NA, nrow(shagasha))

for(i in 1:nrow(shagasha)) {
  if(month(date) == 1 & day(date) == 1) {
    internalCount <- 9
    shagasha$bimonthID[i] <- count
  }
  if(internalCount == 17) {
    internalCount <- 1
    count <- count + 1
  }
  shagasha$bimonthID[i] <- count
  internalCount <- internalCount + 1
  date <- date + 1
}

bimonthEvi <- read.csv("/Users/Tom/Documents/IBI/shagasha/shagashaBimonthEvi.csv", stringsAsFactors = F)
shagasha <- merge(shagasha, bimonthEvi, by = "bimonthID")
shagasha <- shagasha[order(shagasha$date), ]
shagasha <- shagasha[,!(names(shagasha) %in% c("fileDate"))]

wd <- "/Users/Tom/Documents/IBI/shagasha/"

# load the different merra products

merraGrn <- read.csv(paste(wd, "shagashaMerra_grn.csv", sep = ""))
merraRzmc <- read.csv(paste(wd, "shagashaMerra_rzmc.csv", sep = ""))
merraPrmc <- read.csv(paste(wd, "shagashaMerra_prmc.csv", sep = ""))
merraGwettop <- read.csv(paste(wd, "shagashaMerra_gwettop.csv", sep = ""))
merraLai <- read.csv(paste(wd, "shagashaMerra_lai.csv", sep = ""))
merraTsurf <- read.csv(paste(wd, "shagashaMerra_tsurf.csv", sep = ""))
merraSfmc <- read.csv(paste(wd, "shagashaMerra_sfmc.csv", sep = ""))

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

merra$date <- as.Date(merra$date) 
merra <- merge(shagasha, merra, by = "date")

# merge merra data into the shagasha data frame

shagasha$merraRzmc <- merra$rzmc
shagasha$merraPrmc <- merra$prmc
shagasha$merraSfmc <- merra$sfmc
shagasha$merraGrn <- merra$grn
shagasha$merraLai <- merra$lai
shagasha$merraTsurf <- merra$tsurf
shagasha$merraGwettop <- merra$gwettop

# load max correlations from Mulindi
# create MERRA rolling averages based off the max correlations from Mulindi

load("/Users/Tom/Documents/IBI/mulindiMaxCorDf.RData")

shagasha$merraRzmcAvg <- rollapply(merra$rzmc, width = maxCorDf$rzmc, mean, na.rm = T, fill = NA, align = "right")
shagasha$merraPrmcAvg <- rollapply(merra$prmc, width = maxCorDf$prmc, mean, na.rm = T, fill = NA, align = "right")
shagasha$merraSfmcAvg <- rollapply(merra$sfmc, width = maxCorDf$sfmc, mean, na.rm = T, fill = NA, align = "right")
shagasha$merraGrnAvg <- rollapply(merra$grn, width = maxCorDf$grn, mean, na.rm = T, fill = NA, align = "right")
shagasha$merraLaiAvg <- rollapply(merra$lai, width = maxCorDf$lai, mean, na.rm = T, fill = NA, align = "right")
shagasha$merraTsurfAvg <- rollapply(merra$tsurf, width = maxCorDf$tsurf, mean, na.rm = T, fill = NA, align = "right")
shagasha$merraGwettopAvg <- rollapply(merra$gwettop, width = maxCorDf$gwettop, mean, na.rm = T, fill = NA, align = "right")


# load satellite rainfall products and format

tamsat <- read.csv("/Users/Tom/Documents/IBI/shagasha/shagashaTamsat.csv")
tamsat <- rename(subset(tamsat, select = c("date", "meanRainfall")), c("meanRainfall" = "tamsatRainfall"))
tamsat$date <- as.Date(tamsat$date)

chirps <- read.csv("/Users/Tom/Documents/IBI/shagasha/shagashaChirps.csv")
chirps <- rename(subset(chirps, select = c("date", "rain")), c( "rain" = "chirpsRainfall"))
chirps$date <- as.Date(chirps$date)

arc <- read.csv("/Users/Tom/Documents/IBI/shagasha/shagashaArc.csv")
arc <- rename(subset(arc, select = c("date", "rain")), c("rain" = "arcRainfall"))
arc$date <- as.Date(arc$date)

# merge into a data frame with all the rain products

rainDf <- merge(merge(merge(shagasha, tamsat, by = "date", all.x = T, all.y = F),
                      chirps, by = "date", all.x = T, all.y = F), arc, by = "date", all.x = T, all.y = F)

# take the median of the satellite products

avgCol <- c("tamsatRainfall", "chirpsRainfall", "arcRainfall")
rainDf$medianRainfall <- rowMedians(as.matrix(rainDf[, avgCol]))

# merge the median into the shagasha dataframe and create rolling averages for the products

shagasha$medianSatRainfall <- rainDf$medianRainfall
shagasha$medianSatRainfallAvg <- rollapply(rainDf$medianRainfall, width = maxCorDf$medianRainfall, mean, na.rm = T, fill = NA, align = "right")
shagasha$medianSatRainfallSD <- rollapply(rainDf$medianRainfall, width = maxCorDf$medianRainfallSD, sd, na.rm = T, fill = NA, align = "right")

# aggregate data for monthly and weekly levels

merra$medianRainfall <- rainDf$medianRainfall
for (i in 1:(ncol(maxCorDf)-1)) {
  for (j in seq(from = .25, to = 1.5, by = .25)) {
    if (j != 1) {
      #mulindi[, ncol(mulindi) +1] <- rollapply(merra$, width = (maxCorDf$gwettop*j), mean, na.rm = T, fill = NA, align = "right")
      #colnames(mulindi)[ncol(mulindi)] <- paste(names(maxCorDf[i]), j, sep = "_")
      varName <- paste(names(maxCorDf[i]), j, sep = "_")
      #print(varName)
      shagasha[,varName] <- rollapply(merra[,names(maxCorDf[i])], width = floor((maxCorDf[i]*j)), mean, na.rm = T, fill = NA, align = "right")
    }
  }
}



shagashaMonth <- aggregate(. ~ month + year, data = subset(shagasha, select = -c(weekday, date)), FUN = mean, na.rm = T, na.action = NULL )
shagashaWeek <- aggregate(. ~ week + year, data = subset(shagasha, select = -c(weekday, date)), FUN = mean, na.rm = T, na.action = NULL )

# make round down month for weekly aggregate so that it is even months rather than fractions

shagashaWeek$month <- as.factor(floor(shagashaWeek$month))

# remove time variables that shouldn't be used for regressions

shagasha <- subset(shagasha, select = -c(date, year, bimonthID))
shagashaMonth <- subset(shagashaMonth, select = -c(year, bimonthID, week))
shagashaWeek <- subset(shagashaWeek, select = -c(year, bimonthID))

