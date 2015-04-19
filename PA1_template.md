# Reproducible Research: Peer Assessment 1
This research report analzes a sample set of personal movement data using 
an activity monitoring device.  This device provided data at 5 minute intervals 
through out the day. The data consists of two months of data from an anonymous 
individual collected during the months of October and November, 2012 and include
the number of steps taken in 5 minute intervals each day.

To initialize the analysis load the requisite libraries that will potentially
be accessed during the analysis.

```r
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
```

## Loading and preprocessing the data
The dataset for this analysis is Activity monitoring data downloaded to a .zip file.  

After input of the the data a head and tail of the data set is displayed in order to display the column header names and an illustration of the data within the file.

```r
# Read data from .zip'ed file. Specify "character" class so dates don't come
# in as factors.
activity_data <- read.csv(unz("activity.zip","activity.csv"), 
                              colClasses=c("numeric", "character", "numeric"))

head(activity_data); tail(activity_data)
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

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```


## What is mean total number of steps taken per day?
The total number of steps per day are calculated using the dplyr package and piping the data file through a group by of the date variable, selecting the required columns and then summarizing on the steps dropping NA values.

```r
sum_ad <- activity_data %>%
          group_by(date) %>%
          select(steps,date) %>%
          summarise(tot = sum(steps, na.rm = TRUE))
```


A histogram of the count of the number of days in each 5000 step bin, e.g.,  there are about 28 days where the person logged between 10000 and 15000 steps, maybe 2 days in the 20000 to 25000 range and none over 25000 steps. 

```r
g <- ggplot(data=sum_ad, aes(tot)) + 
            geom_histogram(binwidth = 5000,
                           col="red", 
                           aes(fill=..count..)
                          ) +
            labs(title="Number of Times Total Number of Steps Taken Per Day in Interval") +
            labs(x="Step Interval (5000's)", y="Number of Days")
print(g)            
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

The mean of the total number of steps taken per day is: 

```r
mean(sum_ad$tot, na.rm=TRUE) 
```

```
## [1] 9354.23
```
The median of the total number of steps per day is:

```r
median(sum_ad$tot, na.rm=TRUE)
```

```
## [1] 10395
```

For illustrative purposes, the total steps by day are graphed and the mean is indicated by the red solid line and the median by the blue dashed line.

```r
g <- ggplot(data=sum_ad, aes(x=date,y=tot)) + 
            geom_point(shape=3) +
            geom_hline(aes(yintercept=mean(tot, na.rm=TRUE)), colour="red", 
                       linetype="solid") +
            geom_hline(aes(yintercept=median(tot, na.rm=TRUE)), colour="blue", 
                       linetype="dashed") +
            labs(title="Total Number of Steps Taken Per Day") +
            labs(x="Date", y="Total Number of Steps") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
print(g)  
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

## What is the average daily activity pattern?
To get a view of the typical daily behavior of the person, the following time series plot shows by 5-minute interval averaged across all days.

```r
ave_ad <- activity_data %>% 
          group_by(interval) %>% 
          summarize(avesteps = mean(steps, na.rm=TRUE))

g <- ggplot(data=ave_ad, aes(x=interval,y=avesteps)) + 
            geom_line() +
            labs(title="Total Number of Steps Taken Per Day") +
            labs(x="Interval Number", y="Mean Total Number of Steps") 
print(g) 
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

The following interval has the maximum number of steps across the day typically.

```r
ave_ad[which.max(ave_ad$avesteps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval avesteps
## 1      835 206.1698
```


## Imputing missing values
In order to determine if the missing values affect the results, all NA's in the original data set were replaced with the mean across that time period and the summarizations were recomputed and redisplayed in the same manner as above.

```r
length(which(is.na(activity_data$steps))) # Number of NA's
```

```
## [1] 2304
```

```r
length(activity_data$steps)               # Total number of rows
```

```
## [1] 17568
```

```r
steps_imputed <- data.frame(activity_data$steps)
names(steps_imputed) <- "steps_imputed"
steps_imputed[is.na(steps_imputed),] <- floor(tapply(X=activity_data$steps,
                                            INDEX=activity_data$interval,
                                            FUN=mean,na.rm=TRUE))
activity_data <- bind_cols(activity_data, steps_imputed)

sum_ad_i <- activity_data %>%
            group_by(date) %>%
            select(steps_imputed,date) %>%
            summarise(tot = sum(steps_imputed))

mean(sum_ad_i$tot)
```

```
## [1] 10749.77
```

```r
median(sum_ad_i$tot)
```

```
## [1] 10641
```

```r
g <- ggplot(data=sum_ad_i, aes(x=date,y=tot)) + 
            geom_point(shape=3) +
            geom_hline(aes(yintercept=mean(tot)), colour="red", 
                       linetype="solid") +
            geom_hline(aes(yintercept=median(tot)), colour="blue", 
                       linetype="dashed") +
            labs(title="Total Number of Steps Taken Per Day") +
            labs(x="Date", y="Total Number of Steps") + 
            theme(axis.text.x = element_text(angle = 90, hjust = 1, size=6))
print(g) 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 


## Are there differences in activity patterns between weekdays and weekends?
To determine if there was a weekday to weekend difference in user behavior, the data was tagged to show which days were weekend and weekday and then a panel plot is created to compare the results. 

```r
activity_data <- mutate(activity_data, 
                        weekday=ifelse(wday(date)==1|wday(date)==7,
                                       "Weekend","Weekday"))
ave_ad_i <- activity_data %>% 
            group_by(interval,weekday) %>% 
            summarize(avesteps = mean(steps, na.rm=TRUE))

g <- ggplot(data=ave_ad_i, aes(x=interval,y=avesteps)) + 
            geom_line() +
            facet_grid(weekday ~ .) +
            labs(title="Total Number of Steps Taken Per Day") +
            labs(x="Interval Number", y="Mean Total Number of Steps") 
print(g) 
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 

A visual inspection indicates that the person tends to start later on the weekend and tends to have the more strenous step count during the week in the morning.  Perhaps they walk to work. 
