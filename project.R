setwd("/Users/ashisood/Documents/personal/courses/datasciencecoursera/reproducible research/week2")
library(tidyverse)
library(chron)
library(scales)
library(gridExtra)
activity <- as_tibble(read.csv("activity.csv"))
str(activity)
glimpse(activity)
total_steps_per_day <-
        activity %>%
        group_by(date) %>%
        summarise((avg = sum(steps, na.rm = TRUE)))
names(total_steps_per_day)<-c("date", "total.steps")
View(total_steps_per_day)

ggplot(total_steps_per_day, aes(x=total.steps)) +
        geom_histogram() + theme_bw()

ggplot(total_steps_per_day) +
        geom_bar(aes(x=date, y = total.steps), stat = "identity") +
        theme(axis.text.x = element_text(angle = 90, size=8)) +
        ggtitle("Step taken each day")

mean(total_steps_per_day$total.steps, na.rm = TRUE)
median(total_steps_per_day$total.steps, na.rm = TRUE)

total_steps_per_day_ts <-
        activity %>%
        group_by(interval) %>%
        summarise((avg = mean(steps, na.rm = TRUE)))


names(total_steps_per_day_ts)<-c("interval", "mean.steps.ts")
plot(ts(total_steps_per_day_ts$mean.steps.ts))

ggplot(data=total_steps_per_day_ts, aes(x=interval, y=mean.steps.ts)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken") 
max_interval <- total_steps_per_day_ts[which.max(total_steps_per_day_ts$mean.steps.ts),]$interval

gsub("([0-9]{1,2})([0-9]{2})","\\1:\\2",max_interval)
total_steps_per_day_ts$time.of.day<-sprintf("%04d",total_steps_per_day_ts$interval)


total_steps_per_day_ts$time.of.day<-format(strptime(total_steps_per_day_ts$time.of.day, 
                                                     format="%H%M"), 
                                            format = "%H:%M")

names(total_steps_per_day_ts)<-c("interval", "mean.steps.ts", "time.of.day")
total_steps_per_day_ts$hms<-format(total_steps_per_day_ts$time.of.day, 
                                   format = "%H:%M")        
total_steps_per_day_ts$hms <- as.POSIXct(total_steps_per_day_ts$hms, 
                                         format = "%H:%M")

ggplot(total_steps_per_day_ts, aes(hms, mean.steps.ts)) + 
        geom_line()+scale_x_datetime(labels = date_format(format = "%H:%M"))

max_interval <- total_steps_per_day_ts[which.max(total_steps_per_day_ts$mean.steps.ts),]$interval

# Calculate and report the total number of missing values in the 
# dataset (i.e. the total number of rows with 𝙽𝙰s)
sum(is.na(activity$steps))

# Imputing Data
# use the interval mean to update to NA value for that interval
imp_activity<-activity
for (tmp_interval in unique(activity$interval) ) {
        na_interval_index<-which(imp_activity$interval==tmp_interval & is.na(imp_activity$steps))
        imp_activity[na_interval_index,]$steps = 
                total_steps_per_day_ts[total_steps_per_day_ts$interval==tmp_interval,]$mean.steps.ts
        
}

tmp_total_steps_per_day <-
        imp_activity %>%
        group_by(date) %>%
        summarise(sum(steps, na.rm = TRUE))
names(tmp_total_steps_per_day)<-c("date", "total.steps")

glimpse(tmp_total_steps_per_day)
ggplot(tmp_total_steps_per_day, aes(x=total.steps)) +
        geom_histogram(binwidth = 500) + ylab("steps taken per day with binwidth = 500")

tMean = mean(tmp_total_steps_per_day$total.steps, na.rm = TRUE)
tMedian = median(tmp_total_steps_per_day$total.steps, na.rm = TRUE)

imp_activity$week<-
        ifelse(weekdays(as.Date(imp_activity$date)) %in% 
                       c("Saturday", "Sunday"), "weekend", "weekday")
new_mean_steps_per_day_ts <-
        imp_activity %>%
        group_by(interval, week) %>%
        summarise((avg = mean(steps, na.rm = TRUE)))
names(new_mean_steps_per_day_ts)<-c("interval", "week", "mean.steps.day.ts")
View(new_mean_steps_per_day_ts)


ggplot(data=new_mean_steps_per_day_ts, aes(x=interval, y=mean.steps.day.ts)) +
        geom_line() +
        facet_grid(week ~ .) +
        xlab("5-minute interval") +
        ylab("average number of steps taken") 

weekend_mean_steps_per_day_ts <-
        weekend %>%
        group_by(interval) %>%
        summarise((avg = sum(steps, na.rm = TRUE)))
names(weekend_mean_steps_per_day_ts)<-c("interval", "mean.steps.wend.ts")

View(weekend_mean_steps_per_day_ts)


g2<-ggplot(data=weekend_mean_steps_per_day_ts, aes(x=interval, y=mean.steps.wend.ts)) +
        geom_line() +
        xlab("5-minute interval") +
        ylab("average number of steps taken") 

grid.arrange(g1, g2)
