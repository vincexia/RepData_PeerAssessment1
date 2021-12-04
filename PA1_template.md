---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
dt <- read.csv("activity.csv")
dt2 <- dt[complete.cases(dt), ]
```


## What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day

```r
library(tidyverse)
dt3 <- dt2 %>% group_by(date) %>% summarise(sum = sum(steps))
head(dt3)
```

```
## # A tibble: 6 × 2
##   date         sum
##   <chr>      <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```
  
### Make a histogram of the total number of steps taken each day

```r
hist(dt3$sum, main="Histogram of Total Number of Steps Taken Each Day",
    xlab="Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->
  
### Calculate and report the mean and median of the total number of steps taken per day

```r
mean <- mean(dt3$sum)
median <- median(dt3$sum)
```
The mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 10765.

## What is the average daily activity pattern?
### Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
dt4 <- dt2 %>% group_by(interval) %>% summarise(avg = mean(steps))
plot(dt4$interval, dt4$avg, type="l", lty=1, xlab="5-minute interval", 
     ylab="Average number of steps taken aross all days")
```

![](PA1_template_files/figure-html/avg_daily_activity_plot-1.png)<!-- -->

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
interval <- dt4[dt4$avg == max(dt4$avg),]$interval
```
The 5-minute interval 835 contains the maximum number of steps.

## Imputing missing values
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)


```r
df <- as.data.frame(table(complete.cases(dt)))
count <- df[df$Var1 == FALSE, ]$Freq
```
The total number of missing values in the dataset is 2304.

### Devise a strategy for filling in all of the missing values in the dataset.  
Use the mean for that 5-minute interval to fill the missing values.

```r
merged_df <- merge(x=dt, y=dt4, by="interval", all.x=TRUE)

for(i in 1:nrow(merged_df)) {
  row <- merged_df[i,]
  merged_df[i,]$steps <- ifelse(is.na(row$steps), row$avg, row$steps)
}

new_dt <- merged_df %>% select(steps, date, interval) %>% arrange(date)
head(new_dt)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
new_dt1 <- new_dt %>% group_by(date) %>% summarise(sum = sum(steps))
hist(new_dt1$sum, main="Histogram of Total Number of Steps Taken Each Day",
    xlab="Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/new_hist-1.png)<!-- -->

```r
new_mean <- mean(new_dt1$sum)
new_median <- median(new_dt1$sum)
```
The mean and median of the total number of steps taken per day are 1.0766189\times 10^{4} and 1.0766189\times 10^{4}.

These values differ from the estimates from the first part of the assignment. 
The impact is that total daily number of steps increases.

## Are there differences in activity patterns between weekdays and weekends?
### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
tmp <- new_dt %>% mutate(dt_date = as.POSIXlt(date))
tmp2 <- tmp %>% mutate(label = factor((weekdays(tmp$dt_date) %in% weekdays1), 
                                      levels=c(FALSE, TRUE), 
                                      labels=c('weekend', 'weekday'))
                      )
head(tmp2)
```

```
##       steps       date interval    dt_date   label
## 1 1.7169811 2012-10-01        0 2012-10-01 weekday
## 2 0.3396226 2012-10-01        5 2012-10-01 weekday
## 3 0.1320755 2012-10-01       10 2012-10-01 weekday
## 4 0.1509434 2012-10-01       15 2012-10-01 weekday
## 5 0.0754717 2012-10-01       20 2012-10-01 weekday
## 6 2.0943396 2012-10-01       25 2012-10-01 weekday
```
  

### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library(lattice)
new_dt2 <- tmp2 %>% group_by(interval, label) %>% summarise(avg = mean(steps))
xyplot(avg ~ interval | label, 
       data = new_dt2, 
       layout = c(1, 2),
       type="l", 
       lty=1,
       xlab = "Interval",
       ylab = "Number of steps")
```

![](PA1_template_files/figure-html/plot_weekday-1.png)<!-- -->


