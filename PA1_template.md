# Reproducible Research: Peer Assessment 1

## Introduction

This report examines the data from a personal activity monitoring
device that collected measurements at 5 minute intervals throughout the
day. The data consists of two months of data from an anonymous
individual collected during October and November of 2012.

## Loading and preprocessing the data

The data is contained in a single csv file: *activity.csv* and
archived within the zip file: *activity.zip*.


```r
# Unzip the data archive
unzip("activity.zip")

# Load the data
data <- read.csv("activity.csv")
```

Once loaded, the data was modified to convert the date information
into the native Date type and a column was added to indicate the day
of the week the measurement was taken.


```r
# Transform the date column to the Date type
data <- transform(data,date=as.Date(date,format="%Y-%m-%d"))

# Add the day of the week for the given observation 
library(dplyr,warn.conflicts=FALSE)
data <- mutate(data,day_of_week=weekdays(date))
```

## What is mean total number of steps taken per day?

The total number of steps taken per days was determined by grouping
the data by date and summing the number of steps. For this total,
missing measurements were ignored.


```r
# Create a new data frame grouped by date containing the total number of steps
# per day, ignoring missing measurements
steps <- summarize(group_by(data,date),total_steps=sum(steps,na.rm=TRUE))
```
The following histogram shows the total number of steps with a 500 step bin width.

```r
# Create a histogram of the total steps per day with 500 step bins
library(ggplot2)
qplot(total_steps,
      data=steps,
      geom="histogram",
      binwidth=500,
      xlab="Total Steps",
      ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The mean number of steps taken per day was determined by taking the mean of
the *total_steps* measurement.


```r
mean(steps$total_steps,na.rm=TRUE)
```

```
## [1] 9354.23
```

The median number of steps taken per day was determined by taking the median of
the *total_steps* measurement.


```r
median(steps$total_steps,na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

The average daily activity pattern was determined by grouping each
observation by 5 minute interval and averaging the number of
steps. For this activity pattern, missing measurements were ignored.


```r
# Create a new data frame grouped by interval containing the mean steps per
# interval
interval <- summarize(group_by(data,interval),mean_steps=mean(steps,na.rm=TRUE))
```

The following is the daily activity pattern for the mean steps per
interval.


```r
# Create a line plot activity pattern
qplot(interval,
      mean_steps,
      data=interval,
      geom="line",
      xlab="Interval",
      ylab="Mean Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

The interval containing the maximum number of steps:


```r
interval[interval$mean_steps == max(interval$mean_steps),]$interval
```

```
## [1] 835
```

## Inputting missing values

This data set contains a number of incomplete observations.


```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

The number of missing values viewed per measurement:


```r
sapply(data,function(x) sum(is.na(x)))
```

```
##       steps        date    interval day_of_week 
##        2304           0           0           0
```

### Missing Data Strategy

The missing values in the data set were filled in with the average
number of steps for the 5 minute interval occurring on the same day of
the week as the observation missing the steps measurement. For
example, if interval 60 was missing for an observation occuring on a
Monday, the missing value was assigned the mean number of steps
occurring on interval 60 for all Mondays in the data set.


```r
# Create a new data frame containing the mean interval steps for each day of
# the week
replacement <- summarize(group_by(data,day_of_week,interval),
                         mean_steps=mean(steps,na.rm=TRUE))

# Merge the data frames 
stepsComplete <- merge(x=data,y=replacement)

# Reorder by date and interval (helps when viewing)
stepsComplete <- stepsComplete[order(stepsComplete$date,stepsComplete$interval),]

# Replace any NA steps measurement with the mean_steps
stepsComplete[is.na(stepsComplete$steps),]$steps = 
    stepsComplete[is.na(stepsComplete$steps),]$mean_steps

# Group by date and sum total number of steps per day
stepsComplete <- summarize(group_by(stepsComplete,date),total_steps=sum(steps))
```

The following histogram shows the total number of steps with a 500 step bin width.


```r
# Create a histogram of the total steps per day with 500 step bins
library(ggplot2)
qplot(total_steps,
      data=stepsComplete,
      geom="histogram",
      binwidth=500,
      xlab="Total Steps",
      ylab="Count")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png) 

The mean number of steps taken per day was determined by taking the mean of
the *total_steps* measurement.


```r
mean(stepsComplete$total_steps,na.rm=TRUE)
```

```
## [1] 10821.21
```

The median number of steps taken per day was determined by taking the median of
the *total_steps* measurement.


```r
median(stepsComplete$total_steps,na.rm=TRUE)
```

```
## [1] 11015
```

Both the mean and median number of total steps were greater than the
equivalent values for the original data set that ignored missing
measurements.

## Are there differences in activity patterns between weekdays and weekends?

An additional column was added to the data to delineate weekday and
weekend observations. The mean number of steps grouped by interval and
type: weekend or weekday, was used to compare activity patterns.


```r
# Add a new column with either 'weekday' or 'weekend' corresponding to the day
# of the week 
data <- mutate(data,
               type=ifelse(day_of_week %in% c("Saturday","Sunday"),
                           "weekend",
                           "weekday"),
                           type=factor(type))

# Create a data frame with the mean steps grouped by interval and weekend
# or weekday
interval <- summarize(group_by(data,interval,type),
                      mean_steps=mean(steps,na.rm=TRUE))
```

The following activity patterns show that during weekdays there is
more activity earlier in the day compared to the weekend. During the
weekends there is more activity later in the day.


```r
qplot(interval,
      mean_steps,
      data=interval,
      geom="line",
      xlab="Interval",
      ylab="Mean Steps") + facet_grid(type ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 
