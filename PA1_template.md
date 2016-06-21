# Reproducible Research: Peer Assessment 1
#Title: "Assignment 1 for Reproducible Research"
author: "Sudip Goswami"
date: "June 20, 2016"
=====================================================================================

###Introduction

Data for this assignment comes from an activity monitoring device. It contains data collected at 5-minute interval taken over a two-month period in the month of October and November 2012. This data include the number of steps taken in 5-minute interval each day by an anonymous person. 

###Analysis

The variables included in this dataset are:

**steps**: Number of steps taking in a 5-minute interval (missing values are coded as NA)
**date**: The date on which the measurement was taken in YYYY-MM-DD format
**interval**: Identifier for the 5-minute interval in which measurement was taken

This assignment involves analysis that comprises of several steps as descrbed below:


## Loading and preprocessing the data


```r
setwd("C:/Users/juna/Desktop")
data <- read.csv("activity.csv")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## What is mean total number of steps taken per day?

As can be seen from the result of the code below it is 9354.23


```r
data1 <- group_by(data, date)
data2 <- summarize(data1, sum(steps, na.rm = TRUE))
colnames(data2) <- c("Date", "Total_Steps")
mean(data2$Total_Steps)
```

```
## [1] 9354.23
```

## What is the average daily activity pattern?

**Histogram of the total number of steps taken each day**


```r
hist(data2$Total_Steps, xlab = "No of Steps", ylab = "Frequency", breaks = 10, main = "Activity Per Day Histogram")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)

**Mean and median number of steps taken each day.** 
The following code gives the information


```r
summary(data2$Total_Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10400    9354   12810   21190
```
**Time series plot of the average number of steps taken**


```r
data$interval <- factor(data$interval)
data3 <- group_by(data, interval)
data4 <- summarize(data3, steps = mean(steps, na.rm = TRUE))
with(data4, plot(interval, steps, xlab = "Interval", ylab = "Average No. of steps", type = "l", color = "red", main = "Average no. of steps per interval" ))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)
**The 5-minute interval that, on average, contains the maximum number of steps.**

As we can see from the result of the code below, it is 835



```r
mint <- data4[data4$steps == max(data4$steps), ]$interval
mint
```

```
## [1] 835
## 288 Levels: 0 5 10 15 20 25 30 35 40 45 50 55 100 105 110 115 120 ... 2355
```


## Imputing missing values
**Calculate and report the total number of missing values in the dataset.**
Total number of rows with missing value(NA) is 2304


```r
mv <- sum(is.na(data))
mv 
```

```
## [1] 2304
```
**Code to describe and show a strategy for imputing missing data**
Here the missing data is imputed using the mean number of steps for each interval, i.e. for any given interval, the missing value is replaced by the mean number of steps for that interval


```r
impute.mean <-function(x)replace(x,is.na(x), mean(x, na.rm = TRUE))
data5 <- group_by(data, interval)
data6 <- mutate(data5, steps = impute.mean(steps))
```



## Are there differences in activity patterns between weekdays and weekends?

**Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**
This plot is done with lattice package.


```r
library(lattice)
data6$date <- as.Date(data6$date)
weekdays1 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
data6$wDay <- factor((weekdays(data6$date) %in% weekdays1), levels = c(FALSE, TRUE), labels = c("Weekend", "Weekday"))
data9 <- group_by(data6, interval, wDay)
data10 <- summarize(data9, mean_steps =  mean(steps))
xyplot(mean_steps~interval|wDay, layout =c(1,2), data = data10, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)
Clearly there is a difference in activity pattern between weekdays and weekends, however the peak activity seems to be around the same time for both weekdays and weekend however, the peaks are more distributed during the weekend compared to weekdays when the peak activity seems to be between 8 and 9 am in the morning.
