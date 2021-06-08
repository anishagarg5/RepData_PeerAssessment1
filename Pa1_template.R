# Reproducible Research: Peer Assessment 1
            
## Loading and preprocessing the data
unzip(zipfile = "activity.zip")
data <- read.csv("activity.csv")
str(data)


## What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day
steps_per_day <- tapply(data$steps, data$date, FUN = sum, na.rm = TRUE)

### 2. Make a histogram of the total number of steps taken each day
library(ggplot2)
qplot(steps_per_day, binwidth = 2000, xlab = "steps", ylab = "frequency", main = "Total Number Of Steps Taken Per Day") + theme_bw()

### 3. Calculate and report the mean and median of the total number of steps taken per day
mean(steps_per_day, na.rm = TRUE)
## [1] 9354.23 ~ 9354

median(steps_per_day, na.rm = TRUE)
## [1] 10395


## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
library(ggplot2)
average <- aggregate(x = list(steps = data$steps), by = list(interval = data$interval), mean, na.rm = TRUE)
ggplot(average, aes(x = interval, y = steps)) + geom_line() + theme_bw() + xlab("5-minute interval") + ylab("average number of steps taken") + labs(title = "Average Daily Activity Pattern")

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
average[which.max(average$steps), ]
##     interval steps
## 104      835 206.2


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
missing <- is.na(data$steps)
table(missing)
## missing
## FALSE  TRUE 
## 15264  2304
## i.e., 2304 missing

### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Replace each missing value with the mean value of its 5-minute interval because that would be less biased than taking daily averages or daily median.

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
replace_na <- function(steps, interval) {
            replace <- NA
            if (!is.na(steps)) 
                        replace <- c(steps) 
            else 
                        replace <- (average[average$interval == interval, "steps"])
            return(replace)
}
data_no_na <- data
data_no_na$steps <- mapply(replace_na, data_no_na$steps, data_no_na$interval)

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
steps_per_day_no_na <- tapply(data_no_na$steps, data_no_na$date, sum)
qplot(steps_per_day_no_na, binwidth = 2000, xlab = "steps", ylab = "frequency", main = "Total Number Of Steps Taken Per Day") + theme_bw()

mean(steps_per_day_no_na)
## [1] 10766.19 ~ 10766

median(steps_per_day_no_na)
## [1] 10766.19 ~ 10766

## Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with `steps` values `NA` for any `interval`, which are set to 0s by default. However, after replacing missing `steps` values with the mean `steps` of associated `interval` value, these 0 values are replaced with the mean of the steps in the 5-minute interval, which is greater than 0.


## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
week_day_end <- function(date) {
            day <- weekdays(date)
            if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
                        return("weekday") 
            else if (day %in% c("Saturday", "Sunday")) 
                        return("weekend") 
            else 
                        stop("invalid date")
}
data_no_na$date <- as.Date(data_no_na$date)
data_no_na$day <- sapply(data_no_na$date, FUN = week_day_end)

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
average <- aggregate(steps ~ interval + day, data_no_na, mean)
ggplot(average, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + theme_bw() + xlab("5-minute interval") + ylab("number of steps") + labs(title = "Average Weekend Weekday Activity")