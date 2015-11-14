
## libs
library(dplyr)
library(ggplot2)

### Loading and preprocessing the data

unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("numeric","character", "numeric"))
data$date <- as.Date(data$date, format = "%Y-%m-%d")

## What is mean total number of steps taken per day  

total_per_day <- data %>% group_by(date) %>% summarise(total = sum(steps))

total_mean <- mean(total_per_day$total, na.rm = T)
total_median <- median(total_per_day$total, na.rm = T)

  ggplot(data = total_per_day, aes(x=date, y=total)) + geom_bar(stat="identity") +
    geom_hline(yintercept = total_mean, color = "red") +
      geom_hline(yintercept = total_median, color = "blue", linetype = "longdash") +
        xlab("Date") + ylab("Total steps")+ ggtitle("Steps per day")
  

  ggplot(data = total_per_day, aes(x=total)) + geom_histogram() +
    geom_vline(xintercept = total_mean, color = "red") +
      geom_vline(xintercept = total_median, color = "blue", linetype = "longdash") +
        xlab("Steps") + ylab("Count")+ ggtitle("Total number of steps taken each day") 
  
total_mean
total_median  
  
## What is the average daily activity pattern?

act_patern <- data %>% group_by(interval) %>% summarise(avarage = mean(steps, na.rm = T))

top_interval <- (filter(act_patern, avarage == max(act_patern$avarage)) %>% select(interval))[[1,1]]
  
    ggplot(data = act_patern, aes(x = interval, y = avarage)) + geom_line() + geom_vline(xintercept = top_interval, color = "blue", linetype = "dotted", alpha = 0.3)

top_interval  


## Imputing missing values

colSums(is.na(data)) ## !!

#sum(is.na(data$steps))
#sum(is.na(data$interval))
#um(is.na(data$date))

temp_data <- data %>% group_by(date) %>% mutate(miss_data = sum(is.na(steps)))

  summary(temp_data$miss_data)
  unique(temp_data$miss_data)

data_full <- data  

    for (x in 1:nrow(data_full))
    {
      if (is.na(data_full$steps[x]))
      {
        data_full$steps[x] <- (filter(act_patern, interval == data_copy$interval[x]))[[2]]
      }
    }


total_per_day_full <- data_full %>% group_by(date) %>% summarise(total = sum(steps))

total_mean_full <- mean(total_per_day_full$total, na.rm = T)
total_median_full <- median(total_per_day_full$total, na.rm = T)

  ggplot(data = total_per_day_full, aes(x=date, y=total)) + geom_bar(stat="identity") +
    geom_hline(yintercept = total_mean_full, color = "red") +
      geom_hline(yintercept = total_median_full, color = "blue", linetype = "longdash") ## добавить легенду
  

  ggplot(data = total_per_day_full, aes(x=total)) + geom_histogram() +
    geom_vline(xintercept = total_mean_full, color = "red") +
      geom_vline(xintercept = total_median_full, color = "blue", linetype = "longdash")  ## добавить легенду
  
total_mean_full
total_median_full  




## Are there differences in activity patterns between weekdays and weekends?


data_full <- data_full %>% mutate(type = ifelse(as.POSIXlt(date)$wday >= 6,"weekend","weekday"))
data_full$type <- factor(data_full$type)


act_patern_type <- data_full %>% group_by(type, interval) %>% summarise(avarage = mean(steps, na.rm = T))

    ggplot(data = act_patern_type, aes(x = interval, y = avarage)) + geom_line() + facet_grid(type ~ .)






#######################
#######################
#######################


    
colSums(is.na(data))
    


as.POSIXlt(data_full$date[7691])$wday




weekdays(data_full$date, abbreviate = T)

data_full$date[1]
data_full$date[c(1,255,555)]


weekdays(data_full$date[7691])



str(as.POSIXlt(data_full$date[c(1,555,1555)]))



t <- as.Date(data$date[1:5], format = "%Y-%m-%d")

data$date[1:5]
str(t)

str(data)
str(data_full)

tail(data)
data$steps
data$interval
