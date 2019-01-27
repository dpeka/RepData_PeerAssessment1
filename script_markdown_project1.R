library(dplyr)

activity <- read.csv("./data/activity.csv", header=TRUE)


activity_by_date <- activity %>% group_by(date) %>%
      summarise(steps_sum = sum(steps, na.rm = T),
                steps_mean = mean(steps, na.rm = T), 
                steps_median = median(steps, na.rm = T))

#Question - 1

#the mean of total number of steps taken per day
day_mean <- mean(activity_by_date$steps_sum, na.rm = T)
print(paste("The mean number of steps taken per day is", round(day_mean)))

#the median of total number of steps taken per day
day_median <- median(activity_by_date$steps_sum, na.rm = T)
print(paste("The median number of steps taken per day is", round(day_median)))

# Histogram - total steps per day
hist(activity_by_date$steps_sum, main = "Histogram of daily steps", breaks = 10, 
     xlab = "Steps per day", ylab = "Number of days", col = "khaki")

# Each day is divided into 5 minute intervals.  Intervals start at 0 (midnight)
# and go up to 2355.  
activity_by_interval <- activity %>% group_by(interval) %>%
      summarise(steps_sum = sum(steps, na.rm = T),
                steps_mean = mean(steps, na.rm = T), 
                steps_median = median(steps, na.rm = T))

plot(x = activity_by_interval$interval, y = activity_by_interval$steps_mean, 
     type = "l", main = "Average Number of Steps by Day Interval",
     sub = "Day divided into 5 minute intervals",
     xlab = "Time of Day", ylab = "Average Number of Steps")

# Which 5 minute interval, on average, contains the highest average?
max_mean_interval <- activity_by_interval[
      which.max(activity_by_interval$steps_mean), ]$interval
print(paste("The interval that averages the most steps each day is", 
            max_mean_interval))

# Question 2 - Identifying missing values, creating a strategy for imputation
# recalculate mean and median using imputed values
# Empty values
emptyrows <- sum(is.na(activity$steps))
print(paste("There are", emptyrows, "intervals with no steps recorded."))

# We create a function that returns the median of a specific interval
interval_median <- function(x) {
      
      median(subset(activity, interval == x)$steps, na.rm = T)
      
}

# we impute missing values 
# if steps equals "NA", then we use the median number of steps
# of the interval
int_median <- sapply(activity$interval, interval_median)
imputed_activities <- cbind(activity, int_median)

imputed_activities <- imputed_activities %>% 
      mutate(imp_steps = ifelse(is.na(steps), int_median, steps)) %>%
      select(steps, date, interval, imp_steps)

imputed_by_day <- imputed_activities %>% group_by(date) %>%
      summarise(steps_sum = sum(imp_steps))

# How do things change with imputed values?   
#the mean of total number of steps taken per day
day_mean_imputed <- mean(imputed_by_day$steps_sum, na.rm = T)
print(paste("The mean number of steps taken per day is", round(day_mean_imputed)))

#the median of total number of steps taken per day
day_median_imputed <- median(imputed_by_day$steps_sum, na.rm = T)
print(paste("The median number of steps taken per day is", round(day_median_imputed)))

# Histogram - total steps per day
hist(imputed_by_day$steps_sum, main = "Histogram of daily steps", breaks = 10, 
     xlab = "Steps per day", ylab = "Number of days", col = "khaki")

##############
# Differences between weekdays and weekends?

library(lubridate)

wkend_or_wkday <- function(x) {
      
      day_number <- wday(x, week_start = 1)
      if (day_number == 6 | day_number == 7) {
            return("weekend")
      } else {
            return("weekday")
      }
      
}

imputed_activities$day_type <- sapply(imputed_activities$date, wkend_or_wkday)

imp_act_by_interval <- imputed_activities %>% 
      group_by(interval, day_type) %>%
      summarise(steps_sum = sum(imp_steps),
                steps_mean = mean(imp_steps), 
                steps_median = median(imp_steps))

library(ggplot2)

ggplot(imp_act_by_interval, aes(x=interval, y=steps_mean, col=day_type)) +
      geom_line() +
      facet_wrap(.~ day_type, ncol = 1) +
      labs(title = "Average number of steps taken throughout the day", 
           subtitle = "Measured in 5 minute intervals", 
           x = "Time of day", y = "Number of steps") +
      theme(legend.position = "none")

#par(mfrow=c(2,1))    # set the plotting area into a 2*1 array
with(subset(imp_act_by_interval, day_type == "weekday"), 
     plot(x = interval, y = steps_mean, 
     type = "l", main = "Average Number of Steps by Weekday Interval",
     sub = "Day divided into 5 minute intervals",
     xlab = "Time of Day", ylab = "Average Number of Steps"))
with(subset(imp_act_by_interval, day_type == "weekend"), 
     plot(x = interval, y = steps_mean, 
          type = "l", main = "Average Number of Steps by Weekend Interval",
          sub = "Day divided into 5 minute intervals",
          xlab = "Time of Day", ylab = "Average Number of Steps"))


