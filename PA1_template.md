# Reproducible Research: Peer Assessment 1




```r
## Loading and preprocessing the data
library(ggplot2)
getwd()
```

```
## [1] "D:/Jay/Jayateerth/Personal/Business_Analytics_UsingR/ReproducibleResearch/RepData_PeerAssessment1"
```

```r
activity<-read.csv("D:/Jay/Jayateerth/Personal/Business_Analytics_UsingR/ReproducibleResearch/activity.csv")

head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
activity$date <- as.POSIXct(activity$date, "%Y-%m-%d")
weekday <- weekdays(activity$date)
activity <- cbind(activity,weekday)
summary(activity)
```

```
##      steps             date               interval           weekday    
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0   Friday   :2592  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8   Monday   :2592  
##  Median :  0.00   Median :2012-10-31   Median :1177.5   Saturday :2304  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5   Sunday   :2304  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2   Thursday :2592  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0   Tuesday  :2592  
##  NA's   :2304                                           Wednesday:2592
```

```r
## 1. What is mean total number of steps taken per day?
activity_total_steps<-with(activity,aggregate(steps,by=list(date),FUN=sum,na.rm=TRUE))
names(activity_total_steps) <- c("date", "steps")
hist(activity_total_steps$steps, main = "Total number of steps taken per day", xlab = "Total steps taken per day", col = "darkblue", ylim = c(0,20), breaks = seq(0,25000, by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#Mean of the Number of steps taken per day
mean(activity_total_steps$steps)
```

```
## [1] 9354.23
```

```r
#Median of the Number of steps taken per day
median(activity_total_steps$steps)
```

```
## [1] 10395
```

```r
## 2. What is the average daily activity pattern?
#Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
average_daily_activity <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=TRUE)
names(average_daily_activity) <- c("interval", "mean")
plot(average_daily_activity$interval, average_daily_activity$mean, type = "l", col="blue", lwd = 2, xlab="Interval", ylab="Average number of steps", main="Average number of steps per intervals")
```

![](PA1_template_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average_daily_activity[which.max(average_daily_activity$mean), ]$interval
```

```
## [1] 835
```

```r
## 3.Imputing missing values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
imputed_steps<-average_daily_activity$mean[match(activity$interval,average_daily_activity$interval)]

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity_imputed<-transform(activity,steps=ifelse(is.na(activity$steps),yes=imputed_steps,no=activity$steps))
total_steps_imputed <- aggregate(steps ~ date, activity_imputed, sum)
names(total_steps_imputed) <- c("date", "daily_steps")

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

hist(total_steps_imputed$daily_steps, col = "darkblue", xlab = "Total steps per day", ylim = c(0,30), main = "Total number of steps taken each day", breaks = seq(0,25000,by=2500))
```

![](PA1_template_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
#Mean of steps after imputing
mean(total_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```

```r
# Meadian of steps after imputing
median(total_steps_imputed$daily_steps)
```

```
## [1] 10766.19
```

```r
#Difference in Mean before and after imputing
diffMean<-mean(total_steps_imputed$daily_steps)-mean(activity_total_steps$steps)
diffMean
```

```
## [1] 1411.959
```

```r
#Difference in Median before and after imputing
diffMedian<-median(total_steps_imputed$daily_steps)- median(activity_total_steps$steps)
diffMedian
```

```
## [1] 371.1887
```

```r
## 4. Are there differences in activity patterns between weekdays and weekends?
#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))
activity$datetype <- sapply(activity$date, function(x) {
        if (weekdays(x) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (weekdays(x) %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
        })


#Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

activity_by_date <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

