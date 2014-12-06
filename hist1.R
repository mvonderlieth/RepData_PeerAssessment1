# author mvonderlieth
library(dplyr,warn.conflicts = F)
library(tidyr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)

# main logic is in the main() function at the end of file
## support functions
# plot on screen
plotOnScreen <- function(plotFunction,...) {
    p = plotFunction(...)
    print(p)
    return(p)
}

plotJustGen <- function(plotFunction,...) {
    p = plotFunction(...)
    return(p)
}

# plot to png file
plotOnDevice <- function(p) {
    png(filename = "./figure/hist1.png", width = 480, height = 480)
    print(p)
    dev.off()
}


## loading and processing functions
# load the full data source into a 'tmp' if not there already,
#  and once it's there, if not test, use full data, else just use sample of the full data.
# Note also storing into globals so that this method can easily be called repetitively.
loadCsvData <- function(dataFile,test,testSampleSize) {
    if (!exists("FullDataFromSource")) {
        FullDataFromSource <<- read.csv(dataFile)
    }
    
    # for expolatory phase just test with small data set in case full data set is large
    if (!test) {
        WorkingDataFromSource <<- FullDataFromSource
    } else {
        # try to keep the same sample set
        if (!exists("WorkingDataFromSource_sample")) {
            WorkingDataFromSource_sample <<- FullDataFromSource %>% sample_n(testSampleSize)
            WorkingDataFromSource <<- WorkingDataFromSource_sample
        }
    }
}

# build the data set for steps by day with NA
buildDataPerDay <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(datePosix=ymd(date)) %>%
        arrange(datePosix) %>%
        group_by(datePosix) %>%
        summarise(stepsTotal = sum(steps,na.rm=TRUE))
    
    return(d)
}

# build the data set for steps by interval with NA
buildDataPerInterval <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(datePosix=ymd(date)) %>%
        arrange(interval) %>%
        group_by(interval) %>%
        summarise(meanIntervalSteps = mean(steps, na.rm=T))
    
    return(d)
}

# build with NA replace
buildNoNAData <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(steps = ifelse(is.na(steps), as.integer(mean(steps, na.rm=TRUE)), steps))
    return(d)
}

# build with NA replace
buildWeekdayWeekendData <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(weekend = (wday(date) == 1 | wday(date) == 7))
    return(d)
}

# build with NA replace
buildWeekdayData <- function(dataFromSource) {
    d = dataFromSource %>%
        filter(weekend == FALSE)
    return(d)
}

# build with NA replace
buildWeekendData <- function(dataFromSource) {
    d = dataFromSource %>%
        filter(weekend == TRUE)
    return(d)
}

## analysis plot functions
# plot histogtram of mean total number of steps per day
plotHistMeanStepsPerDay <- function(d, addToTitle) {
    mn = mean(d$stepsTotal, na.rm=TRUE)
    md = median(d$stepsTotal, na.rm=TRUE)
    meanText = paste0("Mean = ", as.integer(mn))
    medianText = paste0("Median = ", as.integer(md))
    textDate = ymd(d$date[1])
    textMedianY = max(d$stepsTotal)
    textMeanY = textMedianY - (textMedianY * .05)
    
    p = ggplot(d, aes(x=datePosix, y=stepsTotal)) +
        geom_histogram(stat="identity", color="black", fill="grey", alpha=I(.67)) +
        geom_abline(slope = 0, intercept = mn, color = "red") +
        geom_abline(slope = 0, intercept = md, color = "blue") +
        labs(list(x ="Date",y ="Number of Steps", title=paste("Total Steps Per Day (",addToTitle, ")"))) +
        annotate("text", label = medianText, x = textDate, y = textMedianY, size = 4, colour = "blue", adj = 0) +
        annotate("text", label = meanText, x = textDate, y = textMeanY, size = 4, colour = "red", adj = 0)
    
    return (p)
}

# plot time series of average daily activity pattern
plotTimeSeriesMeanStepsPerDay <- function(d) {
    mx = max(d$meanIntervalSteps, na.rm = TRUE)
    tmp = d$interval[d$meanIntervalSteps == mx]
    mxInterval = tmp[!is.na(tmp)]
    maxText = paste0("Max = ", as.integer(mx), ", Interval = ", mxInterval)
    textMaxX = d$interval[1]
    textMaxY = mx - (mx * .05)
    
    p = ggplot(d, aes(x=interval, y=meanIntervalSteps)) +
        geom_line() +
        geom_abline(slope = 0, intercept = mx, color = "red") +
        labs(list(x ="Interval",y ="Mean of Steps", title="Total Steps Per Interval (NA's not removed)")) +
        annotate("text", label = maxText, x = textMaxX, y = textMaxY, size = 4, colour = "blue", adj = 0)
    
    return (p)
}

## main
main <- function() {
    # when testing data may not represent full set of data.
    # set test to TRUE when exploring data
    dataFile = "./activity.csv"
    
    if (file.exists(dataFile)) {
        # load data as globals
        loadCsvData(dataFile,test=FALSE,testSampleSize=1000)
        
        #build analyis data for mean steps per day
        stepsPerDayData <<- buildDataPerDay(WorkingDataFromSource)
        p = plotOnScreen(plotHistMeanStepsPerDay, stepsPerDayData, "NA's not removed")

        #build analysis data for mean steps per interval
        stepsPerIntervalData <<- buildDataPerInterval(WorkingDataFromSource)
        p = plotOnScreen(plotTimeSeriesMeanStepsPerDay, stepsPerIntervalData)
        
        #build analyis data for imputed missing value
        noNAData <<- buildNoNAData(WorkingDataFromSource)
        #NA's are removed but resuse method
        stepsPerDayNoNAData <<- buildDataPerDay(noNAData)
        p = plotOnScreen(plotHistMeanStepsPerDay, stepsPerDayNoNAData, "NA's removed")
        
        weekdayWeekendData <<- buildWeekdayWeekendData(WorkingDataFromSource)
        #need these or make factor and use lattice?
        weekdayData <<- buildWeekdayData(weekdayWeekendData)
        weekendData <<- buildWeekendData(weekdayWeekendData)    
        weekdayStepsPerIntervalData <<- buildDataPerInterval(weekdayData)
        p1 = plotJustGen(plotTimeSeriesMeanStepsPerDay, weekdayStepsPerIntervalData)
        weekendStepsPerIntervalData <<- buildDataPerInterval(weekendData)
        p2 = plotJustGen(plotTimeSeriesMeanStepsPerDay, weekendStepsPerIntervalData)
        grid.arrange(p1,p2,ncol=1)    
    }
    else {
        warning (paste("The data file",dataFile,"doesn't exist, make sure to set the working directory!"))
    }
}

### run main
main()

#mean(FullDataFromSource$steps, na.rm=TRUE)
#median(FullDataFromSource$steps, na.rm=TRUE)
#sd(FullDataFromSource$steps, na.rm=TRUE)

# Mode <- function(x) {
#     ux <- unique(x)
#     ux[which.max(tabulate(match(x, ux)))]
# }
# Mode(FullDataFromSource$steps)

#proof that median is zero!
# stepsOrdered = FullDataFromSource %>% filter(!is.na(steps)) %>% arrange(steps)
# dim(stepsOrdered)
# stepsOrdered[15264/2,]
#t = WorkingDataFromSource %>% arrange(interval) %>% group_by(interval) %>% summarise(count=n(), intervalStepsMean=mean(steps, na.rm=TRUE), intervalStepsMedian=median(steps, na.rm=TRUE), totalSteps=sum(steps,na.rm=TRUE))
# pairs(airquality)
# fit = lm(Ozone ~ Wind + Solar.R + Temp, data = airquality)
#left off at 2:35