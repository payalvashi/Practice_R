# ggplot2 examples
library(ggplot2) 
library(dplyr)

install.packages('dplyr')

conda install -c r r-essentials

download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip','data.zip',method = "auto", quiet=FALSE)

unzip("data.zip")

my_data <- read.csv("activity.csv")

str(my_data)

SPD <- aggregate(my_data$steps, list(my_data$date), FUN=sum)
colnames(SPD) <- c("Date", "Steps")
SPD

hist(SPD$"Steps",col="red",xlab="Steps",main="Histogram of Step Per Day",breaks = 61,xlim =c(0,24000))

mean_activity <- mean(SPD$Steps, na.rm=TRUE)
median_activity <- median(SPD$Steps, na.rm=TRUE)

mean_activity
median_activity

SPT <- aggregate(steps~interval,data= my_data,FUN=mean, na.action=na.omit)
SPT$time <- SPT$interval/100




h <- ggplot(SPT, aes(time, steps))
h+geom_line(col="brown")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))

ST <- tbl_df(SPT)
# find the column
ST %>% select(time, steps) %>% filter(steps==max(ST$steps))

ACT <- tbl_df(my_data)
# find the column
ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())

my_data$CompleteSteps <- ifelse(is.na(my_data$steps), round(SPT$steps[match(my_data$interval, SPT$interval)],0), my_data$steps)

activityFull <- data.frame(steps=my_data$CompleteSteps, interval=my_data$interval, date=my_data$date)
# see first 10 values of the new dataset
head(activityFull, n=10)

StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")
# draw the histogram
g <- ggplot(StepsPerDayFull, aes(Steps))
g+geom_histogram(boundary=0, binwidth=2500, col="red", fill="lightgrey")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))

mean(StepsPerDayFull$Steps)

median(StepsPerDayFull$Steps)

activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")
# create a variable with weekdays name
activityFull$weekday <- weekdays(activityFull$RealDate)
# create a new variable indicating weekday or weekend
activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(activityFull, n=10)

StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)
# variable time (more comprensible for the graph axis)
StepsPerTimeDT$time <- SPT$interval/100
# draw the line plot
j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkred")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)


