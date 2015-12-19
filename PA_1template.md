---
title: "Peer Assessment 1 - Reproducible Research"
output: html_document
---

### Loading and preprocessing the data.

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
b<-aggregate(steps~date,data=data,sum,simplify = TRUE)
head(b)
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

2. Difference between barplot and histogram and plotting the histogram.

- Barplot : The variable on the x-axis is a categorical variable.  
- Histogram : The variable on the x-axis is a quantitative variable.


We plot the histogram using the hist function from base plotting system.

```r
hist(b$steps,col = "Red",main="Total no. of Steps on a day",xlab = "No. of Steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

3. Mean of the total number of steps taken per day.

```r
mean(b$steps)
```

```
## [1] 10766.19
```

Median of the total number of steps taken per day.

```r
median(b$steps)
```

```
## [1] 10765
```

###Average daily activity pattern

We created a new variable named c which contains the number of steps taken in that interval averaged across all days. Then we plot it using xyplot from lattice package.

```r
data$c<-(aggregate(steps~interval,data=data,mean,simplify = TRUE))$steps
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
nb<-aggregate(steps~date,data=newdata,sum,simplify = TRUE)
head(nb)
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
hist(nb$steps,col = "Blue",main="Total no. of Steps on a day",xlab = "No. of Steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

```r
mean(nb$steps)
```

```
## [1] 10766.19
```

```r
median(nb$steps)
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
newdata$dayt[which(newdata$day=='Saturday' | newdata$day=='Sunday')]="weekend"
newdata$dayt<-as.factor(newdata$dayt)
xyplot(c~interval|dayt,newdata,type="l",xlab="Time Intervals",ylab = "Average no. of steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

From the plots we can see that there is not much difference in the weekends and weekdays as most of the weekends values are imputed in place of NA's and the mean values for each interval is used so plot seems the same and the NA values were there on the weekends only.
