---
title: "Peer Assessment 1 - Reproducible Research"
output: html_document
---

###Loading and preprocessing the data.

The data is downloaded and unzipped in our working directory and stored as "activity.csv". We read the data into a dataframe called data.

```r
data<-read.csv("activity.csv",header=TRUE);
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```
The dataframe has two integer and one factor variable.

###Analysis on daily basis

1. We can calculate the total number of steps taken per day by using tapply. The date variable is already a factor variable so we can use it to split the steps and then apply the sum function.

```r
b<-tapply(data$steps,data$date,sum,na.rm=TRUE,simplify = TRUE)
b
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

2. Difference between barplot and histogram and plotting the histogram.

- Barplot : The variable on the x-axis is a categorical variable.  
- Histogram : The variable on the x-axis is a quantitative variable.


We plot the histogram using the hist function from base plotting system.

```r
hist(b,col = "Red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Mean of the total number of steps taken per day.

```r
mean(b)
```

```
## [1] 9354.23
```

Median of the total number of steps taken per day.

```r
median(b)
```

```
## [1] 10395
```

###Average daily activity pattern

We created a new variable named c which contains the number of steps taken in that interval averaged across all days. Then we plot it using xyplot from lattice package.

```r
data$c<-tapply(data$steps,as.factor(data$interval),mean,na.rm=TRUE,simplify = TRUE)
library(lattice)
xyplot(c~interval,data,type="l",xlab="Time Intervals",ylab = "Average no. of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

Finding the five minute interval that has maximum average number of steps.

```r
res<-data$interval[which(data$c==max(data$c))]
unique(res)
```

```
## [1] 835
```
The five minute interval is 835.


###Imputing missing values

We will be using the average number of steps taken in that interval as the imputed value. We calculate the total number of entries containing NA's values.

```r
mis<-which(complete.cases(data$steps,data$date,data$interval)==FALSE)
length(mis)
```

```
## [1] 2304
```

```r
m<-which(is.na(data$steps)==TRUE)
length(m)
```

```
## [1] 2304
```
The number of entries comtaining NA's is equal to the number os steps entries containing NA's and from summarizing the data we found that the NA's values are in the steps variable only.

Creating a new dataset and replacing the NA values with the mean for that time interval.

```r
newdata<-data
newdata$steps[m]<-newdata$c[m]
```

Now finding the new total steps taken for each day by using tapply and then plotting a histogram and finding mean and median.

```r
nb<-tapply(newdata$steps,newdata$date,sum,na.rm=TRUE,simplify = TRUE)
nb
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##   10766.19     126.00   11352.00   12116.00   13294.00   15420.00 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##   11015.00   10766.19   12811.00    9900.00   10304.00   17382.00 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##   12426.00   15098.00   10139.00   15084.00   13452.00   10056.00 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##   11829.00   10395.00    8821.00   13460.00    8918.00    8355.00 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##    2492.00    6778.00   10119.00   11458.00    5018.00    9819.00 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##   15414.00   10766.19   10600.00   10571.00   10766.19   10439.00 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##    8334.00   12883.00    3219.00   10766.19   10766.19   12608.00 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##   10765.00    7336.00   10766.19      41.00    5441.00   14339.00 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##   15110.00    8841.00    4472.00   12787.00   20427.00   21194.00 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##   14478.00   11834.00   11162.00   13646.00   10183.00    7047.00 
## 2012-11-30 
##   10766.19
```

```r
hist(nb,col = "Blue")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(nb)
```

```
## [1] 10766.19
```

```r
median(nb)
```

```
## [1] 10766.19
```
Oooh, the mean and median results the same. After imputing the NA's the mean and median are increased. The plan for imputing should have been much better.

###Weekdays and Weekends

Creating a new factor variable with two levels weekday and weekend. First we use the strptime and weekdays to find the weekday for each date and then we created a factor variable considering Sunday and Saturday as weekends and rest as weekdays.

```r
newdata$day<-weekdays(strptime(newdata$date,"%Y-%m-%d"))
newdata$dayt<-"weekday"
newdata$dayt[which(newdata$day=="Saturday" | newdata$day=="Sunday")]="weekend"
newdata$dayt<-as.factor(newdata$dayt)
xyplot(c~interval|dayt,newdata,type="l",xlab="Time Intervals",ylab = "Average no. of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

From the plots we can see that there is not much difference in the weekends and weekdays as most of the weekends values are imputed in place of NA's.
