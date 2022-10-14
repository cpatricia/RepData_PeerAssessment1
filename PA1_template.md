---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

##Reading the data from local file

```r
#Set the working directory
setwd("~/Coursera/Reproducible Research_John Hopkins_Uni/activity")

#Read the csv file into Activity object
Activity=read.csv("activity.csv")
```

##Load libraries

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 4.1.3
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 4.1.3
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 4.1.3
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
##Process the data

```r
#Format the date column in Activity to a date object
Activity$date=ymd(Activity$date)
```

##What is the mean total number of steps taken per day?

```r
#Process the dataset to summarize the data by summing up the number of steps, grouped by date
Activity_A=Activity%>%group_by(date)%>%
  summarize(DailyTotal=sum(steps,na.rm=TRUE))

#Plot the daily step histogram. 
ggplot(data=Activity_A,aes(x=DailyTotal))+
  geom_histogram()+
  theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
#The mean daily steps
mean(Activity_A$DailyTotal)
```

```
## [1] 9354.23
```

```r
#the median daily steps
median(Activity_A$DailyTotal)
```

```
## [1] 10395
```


##What is the average daily activity pattern?

```r
#Process the dataset to summarize the data by taking hte mean number of steps, grouped by intervals
Activity_B=Activity%>%group_by(interval)%>%
         summarize(MeanSteps=mean(steps,na.rm=TRUE))

#Plot the data using geom_line
ggplot(data=Activity_B,aes(x=interval,y=MeanSteps))+
  geom_line()+
  theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
#Grab the index for which row has the maximum value of mean steps:
MaxIntervalIndex=which.max(Activity_B$MeanSteps)

#Find the interval with the maximum mean steps:
Activity_B$interval[MaxIntervalIndex]
```

```
## [1] 835
```

##Imputing missing values

```r
#sum up the number of lines that have NA values in the steps variable
sum(is.na(Activity$steps))
```

```
## [1] 2304
```

```r
#Replace missing values with mean steps grouped by interval
Activity_C=Activity%>%group_by(interval)%>%
  mutate(steps=ifelse(is.na(steps),mean(steps,na.rm=TRUE),steps))
         
#Plot the daily step histogram. 
ggplot(data=Activity_C%>%group_by(date)%>%summarize(DailyTotal=sum(steps)),aes(x=DailyTotal))+
  geom_histogram()+
  theme_bw()
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#The mean daily steps
mean((Activity_C%>%group_by(date)%>%summarize(DailyTotal=sum(steps)))$DailyTotal)
```

```
## [1] 10766.19
```

```r
#the median daily steps
median((Activity_C%>%group_by(date)%>%summarize(DailyTotal=sum(steps)))$DailyTotal)
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?

```r
#Create variables to record day of week, and then an identifier variable for whether those days are counted as weekdays or weekends
Activity_D=Activity_C%>%mutate(Weekdays=weekdays(date))%>%
  mutate(WeekID=case_when(
    Weekdays%in%c("Monday","Tuesday","Wednesday","Thursday","Friday")~"weekday",
    Weekdays%in%c("Saturday","Sunday")~"weekend"
  ))

#Plot mean steps grouped by intervals and whether the days are weekdays or weekends
ggplot(Activity_D%>%group_by(WeekID,interval)%>%mutate(MeanSteps=mean(steps,na.rm=TRUE)),aes(x=interval,y=MeanSteps))+
  geom_line()+
  theme_bw()+
  facet_wrap(~WeekID)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

