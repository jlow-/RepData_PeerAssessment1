---
title: "Activity Monitoring"
author: "J"
output: html_document
---
## Assignment
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
Download and unzip the file


```r
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = "data.zip")
data <-  unzip('data.zip',exdir='source data',overwrite=TRUE)
```

Assign source data to respective variables


```r
raw <- read.csv("source data/activity.csv", header =TRUE)
clean_data <- na.omit(raw)
```

Process and clean the raw data by adjusting the header

```r
names(clean_data) <- c("Steps", "Date", "Time")
```
Read column 2 as date

```r
clean_data[,2] <- as.Date(clean_data[,2])
```

## What is mean total number of steps taken per day?

Make a histogram of the total number of steps taken each day


```r
Steps_per_day <- aggregate(Steps ~ Date, data = clean_data, FUN = sum)
png(filename='source data/plot1.png',width=480,height=480,units='px')
hist(Steps_per_day$Steps,main='Histogram of total number of steps per day',xlab='Number of steps',col='blue')
```
Remember to close the file

```r
x<-dev.off()
```

#Calculate and report the mean and median total number of steps taken per day


```r
Mean_steps <- mean(Steps_per_day$Steps)
Mean_steps
```

```
## [1] 10766.19
```

```r
Median_steps <- median(Steps_per_day$Steps)
Median_steps
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis) 


```r
Steps_by_time <- aggregate(Steps ~ Time, data = clean_data, FUN = mean)
png(filename='source data/plot2.png',width=480,height=480,units='px')
plot(x = Steps_by_time$Time,
     y = Steps_by_time$Steps,
     main='Average number of steps, averaged over all days',
     xlab='Time interval',
     ylab='Average number of steps',
     type = 'l',
     col='blue')

# Turn off device
x<-dev.off()
```

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
x<- which.max(Steps_by_time$Steps)
Steps_by_time[x,]
```

```
##     Time    Steps
## 104  835 206.1698
```

##Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
sum(is.na(raw$steps))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The mean for that 5-minute interval over the days will replace the NA values


```r
tidy_raw <- raw
```
Adjust header

```r
names(tidy_raw) <- c("Steps", "Date", "Time")
```
Read column 2 as date

```r
tidy_raw[,2] <- as.Date(tidy_raw[,2])
raw[,2] <- as.Date(raw[,2])
```
Replace NA values with time average over the days


```r
for(i in 1:nrow(tidy_raw)){
  if(is.na(tidy_raw$Steps[i])){
    x <- tidy_raw$Time[i]
    row_id <- which(Steps_by_time$Time == x)
    tidy_raw$Steps[i] <- Steps_by_time$Steps[row_id] 
  }
}
```
Plot the graph


```r
Steps_per_day1 <- aggregate(Steps ~ Date, data = tidy_raw, FUN = sum)
png(filename='source data/plot3.png',width=480,height=480,units='px')
hist(Steps_per_day1$Steps,main='Histogram of total number of steps per day \n(Imputing missing values)',xlab='Number of steps',col='blue')
```

Turn off device

```r
x<-dev.off()
```

Calculate and report the mean and median total number of steps taken per day


```r
Mean_steps1 <- mean(Steps_per_day1$Steps)
Mean_steps1
```

```
## [1] 10766.19
```

```r
Median_steps1 <- median(Steps_per_day1$Steps)
Median_steps1
```

```
## [1] 10766.19
```

## Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
Top 2 indicate the Mean and Median without the imputed data, the last 2 indicate the Mean and Median with imputed data


```r
Mean_steps
```

```
## [1] 10766.19
```

```r
Median_steps
```

```
## [1] 10765
```

```r
Mean_steps1
```

```
## [1] 10766.19
```

```r
Median_steps1
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
Arrange weekday and weekend data

```r
tidy_raw$day <- weekdays(tidy_raw$Date)
tidy_raw$day <- as.factor(tidy_raw$day)

class(tidy_raw$day)
```

```
## [1] "factor"
```

```r
for(i in 1:nrow(tidy_raw)){
  if(tidy_raw$day[i] == "Saturday" || tidy_raw$day[i] == "Sunday"){
    tidy_raw$day_type[i] <- "weekend"
  }
  else{
    tidy_raw$day_type[i] <- "weekday"
  }
}
```


## Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

Now we can compare the two different time series plots.

```r
Steps_by_time1 <- aggregate(Steps ~ Time+day_type, data = tidy_raw, FUN = mean)
names(Steps_by_time1) <- c("Time", "Day_type", "Steps")

library(lattice)

png(filename='source data/plot4.png',width=480,height=480,units='px')
xyplot(Steps ~ Time | Day_type, data = Steps_by_time1, type = "l", xlab = "Time", 
       ylab = "Number of steps", layout = c(1, 2))
```

Turn off device

```r
x<-dev.off()
```
