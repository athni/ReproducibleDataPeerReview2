# Peer assignment-Activity dataset

## Reading the data into activitydata dataset


```r
activitydata <- read.csv("activity.csv", stringsAsFactors = FALSE)
activitydata <-data.frame(date=activitydata$date, steps=activitydata$steps, interval=activitydata$interval)
```

##Calculating the total no. of steps taken each day- 1st remove the NAs and then calculate sum of steps
## Plotting a histogram of total no. of steps taken each day
## Reporting the mean and median of the total no. of steps


```r
sumsteps <- aggregate(activitydata$steps~ activitydata$date, activitydata, sum, na.rm=TRUE)
names(sumsteps) <- c("date", "steps")
```


```r
hist(sumsteps$steps, breaks= seq(from=0, to= 25000, by=2500), col="red", xlab="Total no. of steps", ylab="frequency", main="Histogram of the total no. of steps taken each day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->


```r
mean(sumsteps$steps)
```

```
## [1] 10766.19
```

```r
median(sumsteps$steps)
```

```
## [1] 10765
```

##To make a time series plot, 1st calculating the mean of all the time intervals and then plotting a graph 


```r
meansteps <- aggregate(activitydata$steps~activitydata$interval, activitydata, mean, na.rm=TRUE)
names(meansteps) <- c("interval", "steps")
```


```r
par(mar=rep(2,4))
plot(meansteps$interval, meansteps$steps, type="l", xlab="intervals (in minutes)", ylab="average of steps", main="Time series plot of average no. of steps per intervals", col="red")
```

![](PA1_Template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
maxpos <- which(meansteps$steps==max(meansteps$steps))
maxinterval <- meansteps[maxpos,1]
maxinterval
```

```
## [1] 835
```

## Imputing the missing values. Steps are: first find the no. of NA filled rows in the datafile. Next, find the mean of steps and fill the NA positions with that value


```r
sum(is.na(activitydata$steps))
```

```
## [1] 2304
```

```r
meanOfSteps <-mean(activitydata$steps, na.rm=TRUE)
activitydata[is.na(activitydata)] <- meanOfSteps
```

## Histogram of the new dataset


```r
sumstepsNew <- aggregate(activitydata$steps~ activitydata$date, activitydata, sum, na.rm=TRUE)
names(sumstepsNew) <- c("date", "steps")
```


```r
hist(sumstepsNew$steps, breaks= seq(from=0, to= 25000, by=2500), col="red", xlab="Total no. of steps", ylab="frequency", main="Histogram of the total no. of steps taken each day")
```

![](PA1_Template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
mean(sumstepsNew$steps)
```

```
## [1] 10766.19
```

```r
median(sumstepsNew$steps)
```

```
## [1] 10766.19
```
## The mean and median has differed, rather increased from the orginial set because of imputing the NAs

## Converting date to a date format and then adding 2 new columns day and daytype(weekend or weekday) to it

```r
 activitydata$date <- as.POSIXct(activitydata$date, format="%Y-%m-%d")
sumstepsNew <- data.frame(date=activitydata$date, day=tolower(weekdays(activitydata$date)), steps=activitydata$steps, interval=activitydata$interval)
sumstepsNew <- cbind(sumstepsNew, daytype=ifelse(sumstepsNew$day=="saturday" | sumstepsNew$day=="sunday", "weekend", "weekday"))
sumstepsNew[is.na(sumstepsNew)] <-meanOfSteps
```

##For the time series plot, will 1st calculate the average of the intervals day wise daytype wise


```r
avgsteps <- aggregate(sumstepsNew$steps~ sumstepsNew$daytype+sumstepsNew$day+sumstepsNew$interval, sumstepsNew, mean)
names(avgsteps) <- c("daytype", "day","interval", "steps")
library(lattice)
```


```r
xyplot(steps~interval | daytype, avgsteps, type="l", layout= c(1,2), xlab = "Intervals", ylab="Mean of steps")
```

![](PA1_Template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->










