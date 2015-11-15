wd <- "/Users/Tom/Documents/IBI/"
source('~/Github/IBI/geoFunctions.R')
#source('~/Github/IBI/rainfallCompare.R')
source('~/Github/IBI/bimonthClean.R')
# load data

merraGrn <- read.csv(paste(wd, "merra_grn.csv", sep = ""))
merraRzmc <- read.csv(paste(wd, "merra_rzmc.csv", sep = ""))
merraPrmc <- read.csv(paste(wd, "merra_prmc.csv", sep = ""))
merraGwettop <- read.csv(paste(wd, "merra_gwettop.csv", sep = ""))
merraLai <- read.csv(paste(wd, "merra_lai.csv", sep = ""))
merraTsurf <- read.csv(paste(wd, "merra_tsurf.csv", sep = ""))
merraSfmc <- read.csv(paste(wd, "merra_sfmc.csv", sep = ""))

# merge all the files 
# suppress warnings that the file names are duplicated (irrelevant)

suppressWarnings(merra <- merge(merge(merge(merge(merge(merge(merraGrn, merraRzmc, by = "date"), merraPrmc, by = "date"),
               merraGwettop, by = "date"), merraLai, by = "date"), merraTsurf, by = "date"),
               merraSfmc, by = "date"))

# keep the good data and get rid of the file names

keepCol <- c("date", "grn", "rzmc", "prmc", "gwettop", "lai", "tsurf", "sfmc")

merra$tsurf <- merra$tsurf -273 # surface temp in K, convert to C
merra <- subset(merra, select = keepCol) # keep the good columns

merra$date <- as.Date(merra$date) # change date type for merging
merra <- merge(mulindiTrainDf, merra, by = "date")

# check max correlation for each variable type

mulindiTrainDf$merraRzmc <- merra$rzmc
mulindiTrainDf$merraRzmcAvg <- rollapply(merra$rzmc, width = which.max(MovingAverageCorrelation(merra$rzmc, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindiTrainDf$merraPrmc <- merra$prmc
mulindiTrainDf$merraPrmcAvg <- rollapply(merra$prmc, width = which.max(MovingAverageCorrelation(merra$prmc, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindiTrainDf$merraSfmc <- merra$sfmc
mulindiTrainDf$merraSfmcAvg <- rollapply(merra$sfmc, width = which.max(MovingAverageCorrelation(merra$sfmc, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindiTrainDf$merraGrn <- merra$grn
mulindiTrainDf$merraGrnAvg <- rollapply(merra$grn, width = which.max(MovingAverageCorrelation(merra$grn, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindiTrainDf$merraLai <- merra$lai
mulindiTrainDf$merraLaiAvg <- rollapply(merra$lai, width = which.max(MovingAverageCorrelation(merra$lai, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindiTrainDf$merraTsurf <- merra$tsurf
mulindiTrainDf$merraTsurfAvg <- rollapply(merra$tsurf, width = which.max(MovingAverageCorrelation(merra$tsurf, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")

mulindiTrainDf$merraGwettop <- merra$gwettop
mulindiTrainDf$merraGwettopAvg <- rollapply(merra$gwettop, width = which.max(MovingAverageCorrelation(merra$gwettop, merra$yield, 1, 100, "mean")), mean, na.rm = T, fill = NA, align = "right")


