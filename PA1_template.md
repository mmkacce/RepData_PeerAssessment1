#Reproductible Research
##Course Project
### Martin Markus

---
date: "5 December 2016"
output: html_document
---



###1.Loading and preprocessing the data


```r
setwd("C:/Users/markmar/My Documents")

data <- read.csv("activity.csv")

clear_data <- aggregate(data$steps, by=list(data$date), FUN=sum, na.rm=TRUE)
names(clear_data) <- c("date", "total")
```

###2. What is mean total number of steps taken per day?


Creating the histogram


```r
hist(clear_data$total, 
     main="Daily total Steps",
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     border="black", 
     col="blue",
     ylim=c(0,40))
```

![plot of chunk hist](figure/hist-1.png)


Reporting the mean and the median


```r
mean(clear_data$total)
```

```
## [1] 9354.23
```

```r
median(clear_data$total)
```

```
## [1] 10395
```


###3. What is the average daily activity pattern?

Aggregating, preparing the raw dataset



```r
clear_data3 <- aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
names(clear_data3) <- c("interval", "mean")
```

Creating the line chart


```r
plot(clear_data3$interval, 
     clear_data3$mean,
     type = "l", 
     col="green", 
     lwd = 2,
     xlab="Minutes", 
     ylab="Average number of Steps", 
     main="Average number of steps / intervals")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Determining the best preforming interval across all the days


```r
max <- which(clear_data3$mean == max(clear_data3$mean))
max <- clear_data3[max, 1]
max
```

```
## [1] 835
```

### 4. Imputing missing values

Total number of missing values


```r
nas <- sum(is.na(data$steps))
```

Filling in all of the missing values in the dataset


```r
nas2 <- nas2 <- which(is.na(data$steps))
head(nas2)
```

```
## [1] 1 2 3 4 5 6
```

```r
nas3 <- rep(mean(data$steps, na.rm=TRUE), times=length(nas2))
head(nas3)
```

```
## [1] 37.3826 37.3826 37.3826 37.3826 37.3826 37.3826
```

Create a new dataset replacing the NA's


```r
data[nas2, "steps"] <- nas3
```

Create a histogram 


```r
nadata <- aggregate(data$steps, by=list(data$date), FUN=sum)
names(nadata) <- c("date", "total") 

hist(nadata$total,
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     border="red", 
     col="green",
     ylim=c(0,40),
     main="Total number of steps taken each day without missing values")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

### 5. Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable to decide whether a day is weekday or weekend


```r
whichday <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

data$date <- as.Date(data$date)
data$day <- sapply(data$date, FUN=whichday)
```

Creating a panel plot to compare the weekend and the weekday activity


```r
library(ggplot2)

wow <- aggregate(steps ~ interval + day, data=data, mean)
ggplot(wow, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("Interval") + ylab("Number of steps") 
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)



