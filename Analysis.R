# Eugene Kim
# Reproducible Research
# Week 1 Assignment - analysis

library(dplyr)

#1
if (!file.exists("./activity.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                "./activity.zip")
}

# Unzip and load the full dataset
activity_data <- read.table(unz("activity.zip", "activity.csv"), 
                              header = TRUE, 
                              sep = ",",
                              na.strings = "NA", 
                              colClasses = c("numeric", "character", "numeric")) # Steps, Date, Interval

# update date field to date class
activity_data$date <- as.POSIXct(strptime(activity_data$date, format = "%Y-%m-%d", tz = "UTC"))

# 2
total_steps_per_day <- activity_data %>% group_by(date) %>% summarize(total_steps = sum(steps, na.rm = TRUE))

hist(total_steps_per_day$total_steps, main = "Histogram of Steps Taken Each Day", xlab = "Total Steps", ylab = "Number of Days")

mean(total_steps_per_day$total_steps, na.rm = TRUE)
median(total_steps_per_day$total_steps, na.rm = TRUE)


# 3
avg_steps_per_int <- activity_data %>% group_by(interval) %>% summarize(num_steps = mean(steps, na.rm = TRUE))

plot(avg_steps_per_int$interval, avg_steps_per_int$num_steps, type = "l", 
     main = "Average Number of Steps per Interval", xlab = "Interval", ylab = "Average Number of Steps")

filter(avg_steps_per_int, avg_steps_per_int$num_steps==max(avg_steps_per_int$num_steps))


#4
sum(complete.cases(activity_data) == FALSE)

# Fill in missing date with mean of interval
activity_data_filled <- left_join(activity_data, avg_steps_per_int, by = c("interval", "interval"), type = "left", match = "all")

activity_data_filled <- activity_data_filled %>% mutate(steps_filled = case_when(is.na(activity_data_filled$steps) ~ activity_data_filled$num_steps, 
                           TRUE ~ activity_data_filled$steps)) %>% select(steps_filled, date, interval, steps)


total_steps_per_day_filled <- activity_data_filled %>% 
  group_by(date) %>% 
  summarize(total_steps = sum(steps_filled, na.rm = TRUE))

hist(total_steps_per_day_filled$total_steps, main = "Histogram of Steps Taken Each Day", 
     sub = "(NAs filled with interval mean)", xlab = "Total Steps", ylab = "Number of Days")

mean(total_steps_per_day_filled$total_steps, na.rm = TRUE)
median(total_steps_per_day_filled$total_steps, na.rm = TRUE)



# 5
activity_data_filled_weekday <- activity_data_filled %>% mutate(day_type = as.factor(case_when(weekdays(activity_data_filled$date) == c("Saturday", "Sunday") ~ "weekend",
                              TRUE ~ "weekday"))) %>% select(steps_filled, date, interval, steps, day_type)

avg_steps_per_int_filled <- activity_data_filled_weekday %>% group_by(day_type, interval) %>% summarize(num_steps = mean(steps_filled, na.rm = TRUE))

dev.off()
par(mfcol = c(2,1), oma=c(1,1,2,1))

plot(avg_steps_per_int_filled[avg_steps_per_int_filled$day_type == "weekday",]$interval, 
     avg_steps_per_int_filled[avg_steps_per_int_filled$day_type == "weekday",]$num_steps, 
     type = "l", 
     main = "Weekdays", xlab = "Interval", ylab = "Average Number of Steps")
box(which = "figure", lty = 1)

plot(avg_steps_per_int_filled[avg_steps_per_int_filled$day_type == "weekend",]$interval, 
     avg_steps_per_int_filled[avg_steps_per_int_filled$day_type == "weekend",]$num_steps, 
     type = "l", 
     main = "Weekends", xlab = "Interval", ylab = "Average Number of Steps")
box(which = "figure", lty = 1)

title("Average Number of Steps per Interval", outer = TRUE)

box(which = "outer", lty = 1)




