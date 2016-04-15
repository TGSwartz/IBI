##########################################
### Clean and assemble the datasets to ###
### form the final Sorwate data frames ###
##########################################

# load appriorate functions

source('~/Github/IBI/geoFunctions.R')

libraries <- c("data.table", "xts", "lubridate", "TTR", "plyr", "caTools", "matrixStats")
lapply(libraries, require, character.only = TRUE)

# set first date and end date
# first date from first day of MODIS data
# end date matches Mulindi for consistency in holdout set

firstYear <- 2002
firstMonth <- 7
firstDay <- 4
firstDate <- as.Date(paste(firstYear, firstMonth, firstDay, sep = "-"), format = "%Y-%m-%d")

endYear <- 2015
endMonth <- 05
endDay <- 31 
endDate <- as.Date(paste(endYear, endMonth, endDay, sep = "-"), format = "%Y-%m-%d")

# workout function that creates data for different estates in the sorwate region using the apprioriate data

CreateData  <- function(estateName, recordedWeather = F) {
  
  # create version of the name to be used for naming columns in the dataset
  
  lockedName <- estateName
  
  # read in the productivity data and change of producitivity column to yield
  # keep yield and date only and format date
  
  estateName <- read.csv("/Users/Tom/Documents/IBI/sorwateArea/sorwateAreaProductivity.csv", 
                      stringsAsFactors = F, na.strings = "")
  
  oldNames <- c(paste(tolower(lockedName), "_kg", sep= ""))
  newNames <- c("yield")
  setnames(estateName, old = oldNames, new = newNames)
  
  keepCol <- c("date", "yield")
  estateName <- subset(estateName, select = keepCol)
  
  estateName$date <- as.Date(estateName$date, format = "%d-%b-%y")
  
  # create a dataframe that has NAs for missing producitivity date
  
  estateName <- merge(estateName, data.frame(date = seq.Date(firstDate, 
                                                       endDate, 
                                                       by = 1)), all = T)
  
  estateName <- estateName[estateName$date >= firstDate & estateName$date <= endDate, ]
  
  # format time variables
  
  estateName$month <- as.factor(month(estateName$date))
  estateName$year <- as.factor(year(estateName$date))
  estateName$weekday <- as.factor(weekdays(as.Date(estateName$date)))
  estateName$week <- as.factor(week(estateName$date))
  
  # center and scale yield (for comparable RMSE figures) and replace missing yields with NA
  
  estateName$yield <- as.numeric(scale(estateName$yield, center = T, scale = T))
  
  estateName[is.na(estateName$yield), ]$yield <- NA
  
  
  # create evi bimonthly ID for merging
  # then merge with sorwate EVI file
  
  # make date a date object
  
  date <- as.Date(paste(firstYear, firstMonth, as.character(firstDay), sep = "-")) 
  
  # this function cycles through the dataframe and creates a BimonthID that is used to merge with the EVI records
  # it cycles every 16 days and resets to at beginning of each year and that new EVI lasts until Jan 6
  # new bimonth EVI begins at Jan 7 as it does for the Modis 16-day products
  
  count <- 1
  internalCount <- 1
  bimonthID <- rep(NA, nrow(estateName))
  
  for(i in 1:nrow(estateName)) {
    if(month(date) == 1 & day(date) == 1) {
      internalCount <- 9
      estateName$bimonthID[i] <- count
    }
    if(internalCount == 17) {
      internalCount <- 1
      count <- count + 1
    }
    estateName$bimonthID[i] <- count
    internalCount <- internalCount + 1
    date <- date + 1
  }
  
  # merge the bimonth evi for the sorwate estate with the yield (drop the file date column used for eyeball checking the correct merging)
  
  bimonthEvi <- read.csv(paste("/Users/Tom/Documents/IBI/sorwateArea/", lockedName, "BimonthEvi.csv", sep = ""), stringsAsFactors = F)
  estateName <- merge(estateName, bimonthEvi, by = "bimonthID")
  estateName <- estateName[order(estateName$date), ]
  estateName <- estateName[,!(names(estateName) %in% c("fileDate"))]
  
  # set the working directory for Merra files
  
  wd <- "/Users/Tom/Documents/IBI/sorwateArea/"
  
  # load the different merra products
  
  merraGrn <- read.csv(paste(wd, lockedName, "Merra_grn.csv", sep = ""))
  merraRzmc <- read.csv(paste(wd, lockedName, "Merra_rzmc.csv", sep = ""))
  merraPrmc <- read.csv(paste(wd, lockedName, "Merra_prmc.csv", sep = ""))
  merraGwettop <- read.csv(paste(wd, lockedName, "Merra_gwettop.csv", sep = ""))
  merraLai <- read.csv(paste(wd, lockedName, "Merra_lai.csv", sep = ""))
  merraTsurf <- read.csv(paste(wd, lockedName, "Merra_tsurf.csv", sep = ""))
  merraSfmc <- read.csv(paste(wd, lockedName, "Merra_sfmc.csv", sep = ""))
  
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
  merra <- merge(estateName, merra, by = "date")

  # merge merra data into the sorwate data frame
  
  estateName$merraRzmc <- merra$rzmc
  estateName$merraPrmc <- merra$prmc
  estateName$merraSfmc <- merra$sfmc
  estateName$merraGrn <- merra$grn
  estateName$merraLai <- merra$lai
  estateName$merraTsurf <- merra$tsurf
  estateName$merraGwettop <- merra$gwettop
  
  # load satellite rainfall products and format
  # Tamsat data was not downloading when I was trying to get access for Sorwate area 
  # (after Mulindi and Shagasha due to different times we received the yield data) 
  
  #tamsat <- read.csv(paste("/Users/Tom/Documents/IBI/sorwateArea/", estateName, "Tamsat.csv", sep = ""))
  #tamsat <- rename(subset(tamsat, select = c("date", "meanRainfall")), c("meanRainfall" = "tamsatRainfall"))
  #tamsat$date <- as.Date(tamsat$date)
  
  chirps <- read.csv(paste("/Users/Tom/Documents/IBI/sorwateArea/", lockedName, "Chirps.csv", sep = ""))
  chirps <- rename(subset(chirps, select = c("date", "rain")), c( "rain" = "chirpsRainfall"))
  chirps$date <- as.Date(chirps$date)
  
  arc <- read.csv(paste("/Users/Tom/Documents/IBI/sorwateArea/", lockedName, "Arc.csv", sep = ""))
  arc <- rename(subset(arc, select = c("date", "rain")), c("rain" = "arcRainfall"))
  arc$date <- as.Date(arc$date)
  
  # merge into a data frame with all the rain products
  
  #rainDf <- merge(merge(merge(estateName, tamsat, by = "date", all.x = T, all.y = F),
  #                      chirps, by = "date", all.x = T, all.y = F), arc, by = "date", all.x = T, all.y = F)
  
  rainDf <- merge(
    chirps, arc, by = "date", all.x = T, all.y = F)

  
  # take the median of the satellite products
  
  #avgCol <- c("tamsatRainfall", "chirpsRainfall", "arcRainfall")
  avgCol <- c("chirpsRainfall", "arcRainfall")
  rainDf$medianRainfall <- rowMedians(as.matrix(rainDf[, avgCol]))
  
  # merge the median into the sorwate dataframe and create rolling averages for the products
  
  maxCorDf <- data.frame(rzmc = which.max(abs(MovingAverageCorrelation(merra$rzmc, merra$yield, 1, 100, "mean"))),
                         prmc = which.max(abs(MovingAverageCorrelation(merra$prmc, merra$yield, 1, 100, "mean"))), 
                         sfmc = which.max(abs(MovingAverageCorrelation(merra$sfmc, merra$yield, 1, 100, "mean"))),
                         grn = which.max(abs(MovingAverageCorrelation(merra$grn, merra$yield, 1, 100, "mean"))), 
                         lai = which.max(abs(MovingAverageCorrelation(merra$lai, merra$yield, 1, 100, "mean"))), 
                         tsurf = which.max(abs(MovingAverageCorrelation(merra$tsurf, merra$yield, 1, 100, "mean"))), 
                         gwettop = which.max(abs(MovingAverageCorrelation(merra$gwettop, merra$yield, 1, 100, "mean"))),
                         medianRainfall = which.max(abs(MovingAverageCorrelation(rainDf$medianRainfall, merra$yield, 1, 100, "mean"))),
                         medianRainfallSD = which.max(abs(MovingAverageCorrelation(rainDf$medianRainfall, merra$yield, 2, 100, sd))))
  
  estateName$medianSatRainfall <- rainDf$medianRainfall
  estateName$medianSatRainfallAvg <- rollapply(rainDf$medianRainfall, width = as.numeric(maxCorDf$medianRainfall), mean, na.rm = T, fill = NA, align = "right")
  estateName$medianSatRainfallSD <- rollapply(rainDf$medianRainfall, width = as.numeric(maxCorDf$medianRainfallSD), sd, na.rm = T, fill = NA, align = "right")
  
  # load max correlations from Mulindi
  # create MERRA rolling averages based off the max correlations determined above

  estateName$merraRzmcAvg <- rollapply(merra$rzmc, width = as.numeric(maxCorDf$rzmc), mean, na.rm = T, fill = NA, align = "right")
  estateName$merraPrmcAvg <- rollapply(merra$prmc, width = as.numeric(maxCorDf$prmc), mean, na.rm = T, fill = NA, align = "right")
  estateName$merraSfmcAvg <- rollapply(merra$sfmc, width = as.numeric(maxCorDf$sfmc), mean, na.rm = T, fill = NA, align = "right")
  estateName$merraGrnAvg <- rollapply(merra$grn, width = as.numeric(maxCorDf$grn), mean, na.rm = T, fill = NA, align = "right")
  estateName$merraLaiAvg <- rollapply(merra$lai, width = as.numeric(maxCorDf$lai), mean, na.rm = T, fill = NA, align = "right")
  estateName$merraTsurfAvg <- rollapply(merra$tsurf, width = as.numeric(maxCorDf$tsurf), mean, na.rm = T, fill = NA, align = "right")
  estateName$merraGwettopAvg <- rollapply(merra$gwettop, width = as.numeric(maxCorDf$gwettop), mean, na.rm = T, fill = NA, align = "right")
  
  
  # aggregate data for monthly and weekly levels
  
  #estateNameMonth <- aggregate(. ~ month + year, data = subset(estateName, select = -c(weekday, date)), FUN = mean, na.rm = T, na.action = NULL )
  #estateNameWeek <- aggregate(. ~ week + year, data = subset(estateName, select = -c(weekday, date)), FUN = mean, na.rm = T, na.action = NULL )
  
  # make round down month for weekly aggregate so that it is even months rather than fractions
  
  #estateNameWeek$month <- as.factor(floor(estateNameWeek$month))
  
  # select satellite data into a dataset
  
  satDta <- subset(estateName, select = -c(bimonthID, date))
  
  # select data for estate records after Dec 2006 because estate-recorded rainfall data only exists past that point
  
  estateName <- estateName[estateName$date >= as.Date(paste(2006, 12, 01, sep = "-"), format = "%Y-%m-%d"), ]
  
  # if there is recorded weather data, merge in that weather data with yield
  # else, make the plantDta DF null
  
  if (recordedWeather == T) {
    
  weather <- read.csv("/Users/Tom/Documents/IBI/sorwateArea/sorwateAreaRainfall.csv", 
                      stringsAsFactors = F, na.strings = "")
  
  # keep the columns for the approriate estate from the recorded data
  
  weatherKeepCol <- c(paste(lockedName, "_Pluv", sep= ""),
                      paste(lockedName, "_Temp", sep= ""),
                      paste(lockedName, "_Therm_Norm", sep= ""),
                      paste(lockedName, "_Therm_Min", sep= ""),
                      paste(lockedName, "_Therm_Max", sep= ""),
                      paste(lockedName, "_Hygro_Norm", sep= ""),
                      paste(lockedName, "_Hygro_Min", sep= ""),
                      paste(lockedName, "_Hygro_Max", sep= ""),
                      "date")
  
  weather <- subset(weather, select = weatherKeepCol)
  weather$date <- as.Date(weather$date, format = "%d-%b-%y")
  
  # remove the duplicates that exist in the weather data
  
  weather <- weather[!duplicated(weather$date), ]
  
  estateName <- merge(estateName, weather, by = "date", all.x = T, all.y = F)
  
  # create the rolling average column for the estate for each weather variable
  
  for (i in 1:(length(weatherKeepCol) - 1)) {
    varName <- weatherKeepCol[i]
    newColName <- paste(weatherKeepCol[i], "Avg", sep = "")
    
    # create moving average that at the window width that maximizes correlation with yield
    # if the the entire column is NAs, fill that average column with NAs
    
    if (sum(is.na(estateName[, varName]))/nrow(estateName) != 1) {
    estateName[, newColName] <- 
      rollapply(estateName[, varName], width = as.numeric(which.max(
        abs(MovingAverageCorrelation(estateName[, varName], estateName$yield, 1, 100, 
                                     "mean")))), mean, na.rm = T, fill = NA, align = "right")
    }
    else {
      estateName[, newColName] <- NA
    }
  }
  
  # remove bimonthID and date from dataframe
  # run the keep weather col function (below)
  
  estateName <- subset(estateName, select = -c(bimonthID, date))
  plantDta <- SelectWeatherCol(estateName, lockedName)
  
  # remove weekend days as yield is highly variable (due to non-weather reasons)
  # refactor weekday variable so no weekend levels
  
  plantDta <- plantDta[!(as.character(plantDta$weekday) == "Saturday" | (as.character(plantDta$weekday) == "Sunday")), ]
  plantDta$weekday <- factor(plantDta$weekday)
  }
  else {
    plantDta <- NA
  }
  
  # comment the name of the data so that it can be printed with the model results
  
  comment(plantDta) <- paste(lockedName, "estate data", sep =" ")
  comment(satDta) <- paste(lockedName, "satellite data", sep = " ")
  
  # remove weekend days as yield is highly variable (due to non-weather reasons)
  # refactor weekday variable so no weekend levels
  
  satDta <- satDta[!(as.character(satDta$weekday) == "Saturday" | (as.character(satDta$weekday) == "Sunday")), ]
  satDta$weekday <- factor(satDta$weekday)
  
  write.csv(plantDta, paste("/Users/Tom/Documents/IBI/finalData/", lockedName, "PlantData.csv", sep = ""), row.names = F)
  write.csv(satDta, paste("/Users/Tom/Documents/IBI/finalData/", lockedName, "SatData.csv", sep = ""), row.names = F)
  
  return(list(satDta = satDta, plantDta = plantDta))
}

# function that creates the weather columns for the estate-recorded data

SelectWeatherCol <- function(estateName, lockedName) {
  weatherCol <- c(paste(lockedName, "_Pluv", sep= ""),
                  paste(lockedName, "_Temp", sep= ""),
                  paste(lockedName, "_Therm_Norm", sep= ""),
                  paste(lockedName, "_Therm_Min", sep= ""),
                  paste(lockedName, "_Therm_Max", sep= ""),
                  paste(lockedName, "_Hygro_Norm", sep= ""),
                  paste(lockedName, "_Hygro_Min", sep= ""),
                  paste(lockedName, "_Hygro_Max", sep= ""),
                  paste(lockedName, "_PluvAvg", sep= ""),
                  paste(lockedName, "_TempAvg", sep= ""),
                  paste(lockedName, "_Therm_NormAvg", sep= ""),
                  paste(lockedName, "_Therm_MinAvg", sep= ""),
                  paste(lockedName, "_Therm_MaxAvg", sep= ""),
                  paste(lockedName, "_Hygro_NormAvg", sep= ""),
                  paste(lockedName, "_Hygro_MinAvg", sep= ""),
                  paste(lockedName, "_Hygro_MaxAvg", sep= ""))
  finalCol <- c("yield", "month", "weekday", "week", "year")
  
  # only keep weather data if it has less than 50% missing data
  # this is reduce missing data in the features and thus less observations for training
  # for those columns that meet the criteria, add them to the final columns
  
  for(i in 1:length(weatherCol)) {
    
    if (sum(!is.na(estateName[, weatherCol[i]]))/nrow(estateName) > .5) {
      finalCol <- c(finalCol, weatherCol[i])
      
    }
  }
  plantDta <- subset(estateName, select = finalCol)
  return(plantDta)
}


