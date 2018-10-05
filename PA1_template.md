---
title: 'Reproducible Research: Peer assessment 1'
author: "Beate Nicol"
date: "5 October 2018"
output:
    html_document: 
        keep_md: TRUE
---



#Introduction
This document presents the results of peer assessments 1 of course *Reproducible Research* on Coursera using a single R markdown document that can be processed by knitr and be transformed into an HTML file.   

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.
 
The following steps will be undertaken and questions will be addressed: 

- Loading and preprocessing the data

- mean and median total number of step taken per day

- average daily activity

- daling with missing values

- differences in activity patterns between weekdays and weekends.


##Loading required libraries

```r
library(data.table) # manipulate data 
```

```
## Warning: package 'data.table' was built under R version 3.5.1
```

```r
library(ggplot2) # for plotting figures
```

```
## Warning: package 'ggplot2' was built under R version 3.5.1
```

#Loading and preprocessing the data
The data for this assignment can be downloaded from the course web site:

Dataset: [Activity monitoring data [52K]](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

- steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

- date: The date on which the measurement was taken in YYYY-MM-DD format

- interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

###Instruction:
Download the zip file from the weblink [Activity monitoring data [52K]](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) and save in current working directory.

Read data using read.csv().


```r
data <- read.csv('activity.csv', header = TRUE, sep = ",",
                  colClasses=c("numeric", "character", "numeric"))
```

Convert the "date" filed to Date class and the "invervall" filed to Factor class.


```r
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- as.numeric(data$interval)

str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: num  0 5 10 15 20 25 30 35 40 45 ...
```

#What is mean total number of steps taken per day?
Calculate the total steps per day.


```r
steps_per_day <- aggregate(steps ~ date, data, sum)
colnames(steps_per_day) <- c("date","steps")
head(steps_per_day)
```

```
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Make histogram of total number of steps taken per day, plotted with appropriate bin interval.


```r
ggplot(steps_per_day, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Frequency [Days]") + theme_bw() 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

Calculate mean and median of the number of steps taken per day.


```r
steps_mean   <- round(mean(steps_per_day$steps, na.rm=TRUE), digits = 1)
steps_median <- median(steps_per_day$steps, na.rm=TRUE)
```

Mean number of steps taken: 1.07662\times 10^{4}
Median number of step taken: 1.0765\times 10^{4}

#What is the average daily activity pattern?

Aggregate the steps per interval (i.e. 05, 10, 15…) and also calculates the mean for each interval. This calculated mean value is then plotted as a time series.


```r
steps_interval <- aggregate(steps ~ interval, data=data, FUN=mean)
ggplot(steps_interval, aes(x=interval, y=steps)) + 
       geom_line(color="blue", size=1) +  
        labs(title="Average Daily Activity Pattern", x="Interval", y="Number of steps") +  
        theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

M


```r
max_interval <- steps_interval[which.max(  
        steps_interval$steps),]

max_interval
```

```
##     interval    steps
## 104      835 206.1698
```

 The interval with the maximum number of steps is #835, 206.1698113 steps taken.
 
#Dealing with missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
 

```r
sum(is.na(data))
```

```
## [1] 2304
```
 
The mean of steps for the 05 minute interval is being used to fill in the missing data.

A new data frame **data_filled** will be crated which will replace all NA (from the steps column) with the mean of steps for the 05 minute interval.


```r
data_filled = merge(data, steps_interval, by="interval")
data_filled$steps.x[is.na(data_filled$steps.x)] = data_filled$steps.y[is.na(data_filled$steps.x)]
data_filled$steps.y <- NULL
colnames(data_filled) <- c("interval", "steps", "date")

library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.5.1
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:data.table':
## 
##     between, first, last
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

```r
data_filled <- arrange(data_filled, date)
```

```
## Warning: package 'bindrcpp' was built under R version 3.5.1
```

```r
head(data_filled)
```

```
##   interval     steps       date
## 1        0 1.7169811 2012-10-01
## 2        5 0.3396226 2012-10-01
## 3       10 0.1320755 2012-10-01
## 4       15 0.1509434 2012-10-01
## 5       20 0.0754717 2012-10-01
## 6       25 2.0943396 2012-10-01
```

To plot updated histogram it is necessary to recalculate the aggregation of steps with the new data.

```r
steps_per_day2 <- aggregate(steps ~ date, data_filled, sum)
colnames(steps_per_day2) <- c("date","steps")
head(steps_per_day2)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
ggplot(steps_per_day2, aes(x = steps)) + 
       geom_histogram(fill = "blue", binwidth = 1000) + 
        labs(title="Histogram of Steps Taken per Day", 
             x = "Number of Steps per Day", y = "Frequency [Days]") + theme_bw() 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Recalculate the mean and medium total number of steps taken per day.


```r
steps_mean2 <- mean(steps_per_day2$steps)
steps_median2 <- median(steps_per_day2$steps)

steps_mean2
```

```
## [1] 10766.19
```

```r
steps_median2
```

```
## [1] 10766.19
```
Mean number of steps taken: 1.0766189\times 10^{4}
Median number of step taken: 1.0766189\times 10^{4}

Analysis:  The values of mean and median are now identical wheras before the median was slightly lower than the mean.


#Are there differences in activity patterns between weekdays and weekends?

create a function to classify the date variable either as weekend or weekday.


```r
daytype <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}

data_filled_date <- class(date)
data_filled$daytype <- as.factor(sapply(data_filled$date, daytype))

head(data_filled)
```

```
##   interval     steps       date daytype
## 1        0 1.7169811 2012-10-01 weekday
## 2        5 0.3396226 2012-10-01 weekday
## 3       10 0.1320755 2012-10-01 weekday
## 4       15 0.1509434 2012-10-01 weekday
## 5       20 0.0754717 2012-10-01 weekday
## 6       25 2.0943396 2012-10-01 weekday
```

Making a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 




```r
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval,
                            data=data_filled,
                            subset=data_filled$daytype==type,
                            FUN=mean)
    plot(steps.type, type="l", main=type)
    
}
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->![](PA1_template_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

Analysis: There are significant differences in the activity patterns between weekend and weekdays. While during the weekday activity is concentrated around intervals 700 - 900 during weekend activity is more evently distributed throughout the day.
