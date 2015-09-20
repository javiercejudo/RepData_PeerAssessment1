# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

#### Load the data (i.e. read.csv())


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

#### Process/transform the data (if necessary) into a format suitable for your analysis


```r
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

#### Calculate the total number of steps taken per day


```r
dataPerDay <- aggregate(steps ~ date, data, sum, na.action = na.omit)
```

#### Make a histogram of the total number of steps taken each day


```r
library(ggplot2)

ggplot(dataPerDay, aes(x = steps)) +
	geom_histogram(binwidth = 2000) +
	ggtitle("Total number of steps taken each day") +
	xlab("Steps (binwidth 2000)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

#### Calculate and report the mean and median of the total number of steps taken per day


```r
mean(dataPerDay$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(dataPerDay$steps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

#### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
dataPerInterval <- aggregate(steps ~ interval, data, mean, na.action = na.omit)

ggplot(dataPerInterval, aes(interval, steps)) +
	geom_line() +
	ggtitle("Average number of steps taken, averaged across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
dataPerInterval[which.max(dataPerInterval$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

#### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
nrow(data[is.na(data$steps),])
```

```
## [1] 2304
```

#### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Missing values will be replaced with the mean for their 5-minute interval.

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
dataWithoutNAs <- data

for(i in seq(nrow(dataWithoutNAs))) {
	if (is.na(dataWithoutNAs[i,]$steps)) {
		dataWithoutNAs[i,]$steps <- dataPerInterval[dataPerInterval$interval == dataWithoutNAs[i,]$interval,]$steps
	}
}

str(dataWithoutNAs)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day


```r
dataPerDayWithoutNAs <- aggregate(steps ~ date, dataWithoutNAs, sum, na.action = na.omit)

ggplot(dataPerDayWithoutNAs, aes(x = steps)) +
	geom_histogram(binwidth = 2000) +
	ggtitle("Total number of steps taken each day") +
	xlab("Steps (binwidth 2000)")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(dataPerDayWithoutNAs$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(dataPerDayWithoutNAs$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean is the same and the median varied slightly.

## Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
dataWithWeekdaySplit <- data

dataWithWeekdaySplit$weekday <- rep("weekday", nrow(data))

for(i in seq(nrow(dataWithWeekdaySplit))) {
	if (weekdays(dataWithWeekdaySplit[i,]$date) %in% c('Saturday', 'Sunday')) {
		dataWithWeekdaySplit[i,]$weekday <- "weekend"
	}
}

dataWithWeekdaySplit$weekday <- as.factor(dataWithWeekdaySplit$weekday)
```

#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)


```r
ggplot(dataWithWeekdaySplit, aes(interval, steps)) +
	facet_grid(. ~ weekday) +
	geom_line() +
	ggtitle("Time series plot of the 5-minute interval across all weekday days or weekend days")
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 
