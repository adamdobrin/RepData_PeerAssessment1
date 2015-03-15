
repdata_peer1 <- function(){
  # Load the data
  data <- loadData()
  
  # Calculate the total number of steps taken per day
  stepsPerDayTotal <- aggregate(steps ~ date, data, sum)
  
  # Make a histogram of the total number of steps taken per day
  hist(stepsPerDayTotal$steps, 
       breaks = 20, 
       main = 'Total Number of Steps Taken Per Day', 
       xlab = 'Total Number of Steps', 
       col = 'red')
  
  # Calculate and report the mean and median of the total number of steps taken per day
  stepsPerDayMean <- mean(stepsPerDayTotal$steps, na.rm = TRUE)
  stepsPerDayMedian <- median(stepsPerDayTotal$steps, na.rm = TRUE)
  
  # Calculate the mean number of steps taken per interval (omitting NA)
  stepsPerIntervalMean <- aggregate(steps ~ interval, data, mean)
  
  # Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days
  plot(stepsPerIntervalMean$interval,
       stepsPerIntervalMean$steps,
       type = 'l',
       main = 'Average Steps by 5-Minute Time Interval',
       xlab = '5-Minute Time Interval',
       ylab = 'Average Number of Steps',
       col = 'blue'
       )
  
  # Calculate which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps
  intervalWithMaxAverage <- stepsPerIntervalMean$interval[which.max(stepsPerIntervalMean$steps)]
  
  # Calculate the total number of missing values in the dataset (i.e. the total number of rows with NAs)
  missingValuesCount <- sum(is.na(data$steps))
  
  # Create a new dataset that is equal to the original dataset but with the missing data filled in
  dataNoNA <- transform(data, 
                        steps = ifelse(is.na(data$steps), 
                                       stepsPerIntervalMean$steps[match(data$interval, stepsPerIntervalMean$interval)], 
                                       data$steps
                        )
  )
  
  # Calculate the total number of steps taken per day from data with NA removed
  stepsPerDayTotalNoNA <- aggregate(steps ~ date, dataNoNA, sum)
  
  # Make a histogram of the total number of steps taken per day
  hist(stepsPerDayTotalNoNA$steps, 
       breaks = 20, 
       main = 'Total Number of Steps Taken Per Day (NA Removed)', 
       xlab = 'Total Number of Steps', 
       col = 'red',
       ylim = c(0, 20)
       )
  
  # Calculate and report the mean and median of the total number of steps taken per day
  stepsPerDayMeanNoNA <- mean(stepsPerDayTotalNoNA$steps, na.rm = TRUE)
  stepsPerDayMedianNoNA <- median(stepsPerDayTotalNoNA$steps, na.rm = TRUE)
  
  # Create a new dataset with a day type field to indicate weekday or weekend 
  dataPlusDayType <- data
  dataPlusDayType$daytype <- ifelse(weekdays(as.Date(data$date)) %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  
  # Calculate the mean number of steps taken per interval and day type (omitting NA)
  stepsPerIntervalAndDayTypeMean <- aggregate(steps ~ interval + daytype, dataPlusDayType, mean)
  
  # Load the lattice library and make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days, per day type
  library(lattice)
  
  xyplot(stepsPerIntervalAndDayTypeMean$steps ~ stepsPerIntervalAndDayTypeMean$interval|stepsPerIntervalAndDayTypeMean$daytype,
         main="Average Steps per Day by Interval",
         xlab="5-Minute Time Interval", 
         ylab="Average Number of Steps",
         layout=c(1, 2), 
         type="l")
}

loadData <- function(){
  # Unzip file if necessary
  zipName <- "activity.zip"
  if (file.exists(zipName)) {
    unzip(zipName)
  } else {
    zipName <- paste("repdata_data_", zipName, sep = "")
    if (file.exists(zipName)) {
      unzip(zipName)
    }
  }
  
  # Load data if found
  if (file.exists("activity.csv")) {
    data <- read.csv("activity.csv")
  } else {
    print("Data file not found. Returning empty data frame.")
    data <- data.frame()
  }
  return(data)
}