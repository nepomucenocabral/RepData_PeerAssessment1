---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document: 
    keep_md: true
---

### Loading and preprocessing the data

**1. Load the data (i.e. read.csv())**


```r
activity=read.csv("activity.csv", na.strings="NA")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

**2. Process/transform the data (if necessary) into a format suitable for your analysis**


```r
activity$date=as.Date(activity$date)
class(activity$date)
```

```
## [1] "Date"
```

### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

**1. Calculate the total number of steps taken per day**


```r
totalsteps=tapply(activity$steps, activity$date, sum, na.rm=TRUE)
print(totalsteps)
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

**2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day**


```r
hist(totalsteps, xlab="Total steps taken", ylab="Number of days by amount of steps",
     main="Amount of days for total steps taken")
```

![plot of chunk histogram1](figure/histogram1-1.png) 

**3. Calculate and report the mean and median of the total number of steps taken per day**


```r
mean(totalsteps)
```

```
## [1] 9354.23
```

```r
median(totalsteps)
```

```
## [1] 10395
```

### What is the average daily activity pattern? 
**1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
intervals=aggregate.data.frame(activity$steps, list(activity$interval), mean, na.rm=TRUE)
plot(intervals, type="l", main="Average daily pattern",
     xlab="5-minute interval", ylab="Average number of steps taken")
```

![plot of chunk time series 1](figure/time series 1-1.png) 

**2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
print(intervals$Group.1[max(intervals$x)])
```

```
## [1] 1705
```

### Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

**1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**


```r
sum(!complete.cases(activity))
```

```
## [1] 2304
```

**2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**  
For this part, I've decided to use the mean for the 5-minute interval, averaged for all the days in the dataset. These values are already stored in the dataframe named 'intervals', created before.

I've chosen this approach due to the large amount of variation between different moments within a day - shown in a graph above with the average daily pattern over the 5-minute interval.

**3. Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
colnames(intervals)=c("interval","mean")
activity.input=merge(activity, intervals, by="interval")

for(i in 1:nrow(activity.input)) {
    if(is.na(activity.input$steps[i])==TRUE) {
        activity.input$steps[i]=activity.input$mean[i]
    }
}

activity.input$mean=NULL
summary(activity.input)
```

```
##     interval          steps             date           
##  Min.   :   0.0   Min.   :  0.00   Min.   :2012-10-01  
##  1st Qu.: 588.8   1st Qu.:  0.00   1st Qu.:2012-10-16  
##  Median :1177.5   Median :  0.00   Median :2012-10-31  
##  Mean   :1177.5   Mean   : 37.38   Mean   :2012-10-31  
##  3rd Qu.:1766.2   3rd Qu.: 27.00   3rd Qu.:2012-11-15  
##  Max.   :2355.0   Max.   :806.00   Max.   :2012-11-30
```

**4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**


```r
totalsteps.input=tapply(activity.input$steps, activity$date, sum, na.rm=TRUE)

hist(totalsteps.input, xlab="Total steps taken (missing values inputed)", ylab="Number of days by amount of steps",
     main="Amount of days for total steps taken")
```

![plot of chunk histogram2](figure/histogram2-1.png) 

```r
mean(totalsteps.input)
```

```
## [1] 10766.19
```

```r
median(totalsteps.input)
```

```
## [1] 10351.62
```

The values differ from the estimates from the first part of the assignment. The dataset with inputed values reports a higher mean and a slightly smaller median.

### Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

**1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.**


```r
activity.input$day=weekdays(activity.input$date)
for(i in 1:nrow(activity.input)) {
    if(activity.input$day[i]=="Sábado" | activity.input$day[i]=="Domingo") { 
        activity.input$day[i]="weekend"
    }
    else { activity.input$day[i]="weekday"}
}

activity.input$day=as.factor(activity.input$day)
class(activity.input$day)
```

```
## [1] "factor"
```

**2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.**


```r
library(ggplot2)

weekdays=aggregate.data.frame(activity.input$steps, list(activity.input$day, activity.input$interval), mean)
colnames(weekdays)=c("day", "interval", "steps")

g=ggplot(weekdays, aes(interval, steps))
g+geom_line()+facet_wrap(~ day, nrow=2, ncol=1)+labs(x="Interval")+labs(y="Average number of steps")+labs(title="Patterns for weekdays and weekends")
```

![plot of chunk panel](figure/panel-1.png) 
