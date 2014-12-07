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

# plot on screen
justPlot <- function(plotFunction,...) {
    plotFunction(...)
}

## loading and processing functions
# load the full data source into a 'tmp' if not there already,
#  and once it's there, if not test, use full data, else just use sample of the full data.
# Note also storing into globals so that this method can easily be called repetitively.
loadCsvData <- function(zipFile,dataFile,test,testSampleSize) {
    unzip(zipFile)
    
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
        mutate(datePosix = ymd(date)) %>%
        arrange(interval) %>%
        group_by(interval) %>%
        summarise(meanIntervalSteps = mean(steps, na.rm=T))
    
    return(d)
}

# convert with NA replace
convertStepsToNoNAData <- function(dataFromSource,intervalData) {
    d = dataFromSource %>%
        mutate(steps = ifelse(is.na(steps), intervalData$meanIntervalSteps, steps))
    return(d)
}


# append weekend or weekday factor
appendWeekdayWeekendData <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(weekend = ifelse(wday(date) == 1 | wday(date) == 7,"weekend","weekday"))
    return(d)
}

# build the data set for steps by interval
buildDataPerIntervalWeekend <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(datePosix = ymd(date)) %>%
        arrange(interval,weekend) %>%
        group_by(interval,weekend) %>%
        summarise(meanIntervalSteps = mean(steps, na.rm=T))

    return(d)
}

## analysis plot functions
# plot histogtram of mean total number of steps per day
plotHistMeanStepsPerDay <- function(d, addToTitle = "", numBreaks=10) {
    mn = as.integer(mean(d$stepsTotal, na.rm=TRUE))
    md = as.integer(median(d$stepsTotal, na.rm=TRUE))
    meanText = paste0("Mean = ", mn)
    medianText = paste0("Median = ", md)
    mainText = paste0("Histogram ",addToTitle)
    xmin = min(d$stepsTotal)
    xmax = max(d$stepsTotal)
    xdist = c(xmin,xmax)
    stepsCol = "#0000FF88"

    hist(d$stepsTotal, xlim=xdist, xlab="Steps Per Day", main=mainText, col=stepsCol, ylim=c(0,20), breaks=numBreaks)
    legend("topright", legend=c(meanText,medianText), fill=c("red","blue"), box.lwd = 0, box.col = "red")
    abline(v=c(mn,md),col=c("red","blue"))
}

# plot time series of average daily activity pattern
plotTimeSeriesMeanStepsPerDay <- function(d) {
    mx = max(d$meanIntervalSteps, na.rm = TRUE)
    tmp = d$interval[d$meanIntervalSteps == mx]
    mxInterval = tmp[!is.na(tmp)]
    maxText = paste0("Max = ", as.integer(mx), " at interval = ", mxInterval)
    textMaxX = d$interval[1]
    textMaxY = mx - (mx * .05)
    
    p = ggplot(d, aes(x=interval, y=meanIntervalSteps)) +
        geom_line() +
        geom_abline(slope = 0, intercept = mx, color = "blue") +
        labs(list(x ="Interval",y ="Mean of Steps", title="Total Steps Per Interval (NA's not replaced)")) +
        annotate("text", label = maxText, x = textMaxX, y = textMaxY, size = 4, colour = "blue", adj = 0)
    
    return (p)
}

# plot time series of activity patterns between weekdays and weekends
plotTimeSeriesWeekend <- function(d) {
    p = ggplot(d, aes(x=interval, y=meanIntervalSteps)) +
        geom_line() +
        facet_grid(weekend ~ .) +
        labs(list(x ="Interval",y ="Mean of Steps", title="Total Steps Per Interval Weekday vs Weekend"))
    
    return (p)
}

## main
main <- function() {
    # when testing data may not represent full set of data.
    # set test to TRUE when exploring data
    zipFile = "./activity.zip"
    
    if (file.exists(zipFile)) {
        
        # load data as globals
        loadCsvData(zipFile,dataFile="./activity.csv",test=FALSE,testSampleSize=1000)
        
        # build analyis data for mean steps per day and plot
        stepsPerDayData <<- buildDataPerDay(WorkingDataFromSource)
        addToTitleText = paste0("With NA's, number of breaks=",29)
        justPlot(plotHistMeanStepsPerDay, stepsPerDayData, addToTitle=addToTitleText, numBreaks=29)
        
        # build analysis data for mean steps per interval and plot
        stepsPerIntervalData <<- buildDataPerInterval(WorkingDataFromSource)
        p = plotOnScreen(plotTimeSeriesMeanStepsPerDay, stepsPerIntervalData)
        
        # build analyis data for imputed missing value and plot
        noNAData <<- convertStepsToNoNAData(WorkingDataFromSource,stepsPerIntervalData)
        stepsPerDayNoNAData <<- buildDataPerDay(noNAData)
        addToTitleText = paste0("With ",sum(is.na(WorkingDataFromSource$steps))," NA's replaced with mean, number of breaks=",29)
        p = justPlot(plotHistMeanStepsPerDay, stepsPerDayNoNAData, addToTitle=addToTitleText, numBreaks=29)

        # build analyis data for weekend vs weekday and plot
        weekdayWeekendData <<- appendWeekdayWeekendData(WorkingDataFromSource)
        weekdayWeekendStepsPerIntervalData <<- buildDataPerIntervalWeekend(weekdayWeekendData)
        p = plotOnScreen(plotTimeSeriesWeekend, weekdayWeekendStepsPerIntervalData)
        
    } else {
        warning (paste("The data file",zipFile,"doesn't exist, make sure to set the working directory!"))
    }
}

### run main
main()
# knit2html("PA1_template.Rmd")
