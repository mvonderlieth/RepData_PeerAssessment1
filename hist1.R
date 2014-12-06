# author mvonderlieth
library(dplyr,warn.conflicts = F)
library(tidyr)
library(lubridate)
library(ggplot2)

# main logic is in the main() function at the end of file
## support functions
# make plot
plotHistMeanStepsPerDay <- function(d,mn,md) {
    meanText = paste("Mean =", as.integer(mn))
    medianText = paste("Median =", as.integer(md))
    textDate = ymd(d$date[1])
    textMedianY = max(d$stepsTotal)
    textMeanY = textMedianY - (textMedianY * .075)
    
    p = ggplot(d, aes(x=datePosix, y=stepsTotal)) +
        #         geom_smooth() +
        # #         geom_line() +
        geom_histogram(stat="identity", color="black", fill="grey", alpha=I(.67)) +
        geom_abline(slope = 0, intercept = mn, color = "red") +
        geom_abline(slope = 0, intercept = md, color = "blue") +
        labs(list(x ="Date",y ="Number of Steps", title="Total Steps Per Day (NA's not removed)")) +
        annotate("text", label = medianText, x = textDate, y = textMedianY, size = 4, colour = "blue", adj = 0) +
        annotate("text", label = meanText, x = textDate, y = textMeanY, size = 4, colour = "red", adj = 0)
    
    
    return (p)
}

# plot on screen
plotOnScreen <- function(plotFunction,...) {
    p = plotFunction(...)
    print(p)
    return(p)
}

# plot to png file
plotOnDevice <- function(p) {
    png(filename = "./figure/hist1.png", width = 480, height = 480)
    print(p)
    dev.off()
}

# build the data set we need
buildNAData <- function(dataFromSource) {
    d = dataFromSource %>%
        group_by(date) %>%
        summarise(stepsTotal = sum(steps,na.rm=TRUE)) %>%
        mutate(datePosix=ymd(date))
    
    return(d)
}

buildNoNAData <- function(dataFromSource) {
    d = dataFromSource %>%
        mutate(steps = ifelse(is.na(steps), as.integer(mean(steps, na.rm=TRUE)), steps))
    return(d)
}

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

## main
main <- function() {
    # when testing data may not represent full set of data.
    # set test to TRUE when exploring data
    dataFile = "./activity.csv"
    test = FALSE
    
    if (file.exists(dataFile)) {
        # load data as globals
        loadCsvData(dataFile,test,1000)
        
        #         par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
        
        #build analyis data
        # want this to be a global so I can view it after a run
        analysisNAData <<- buildNAData(WorkingDataFromSource)
        stepsMean = mean(analysisNAData$stepsTotal, na.rm=TRUE)
        stepsMedian = median(analysisNAData$stepsTotal, na.rm=TRUE)
        
        #plot on screen
        p = plotOnScreen(plotHistMeanStepsPerDay, analysisNAData, stepsMean, stepsMedian)
        
        #         analysisNoNAData <<- buildNoNAData(WorkingDataFromSource)
        #         analysisData <<- buildData(analysisNoNAData)
        #         p = plotOnScreen(analysisData)
        
        # create plot on file device, close when done
#         plotOnDevice(p)
        
        #         par(mfrow = c(1, 1))
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