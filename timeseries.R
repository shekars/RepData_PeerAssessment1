library(dplyr)

#read the activity.csv file
activity <- read.csv2("activity.csv", sep=",", na.strings = "NA")

#define the data type of the fields
activity$steps <- as.numeric(activity$steps)
activity$interval <- as.numeric(activity$interval)
activity$date <- as.Date(activity$date)

#group by interval
intervalGroupByActivity <- group_by(activity,  interval)

#calulate avg no. of steps
avgStepsPerInterval <- summarize(intervalGroupByActivity, avg_steps=mean(na.omit(steps)))
avgStepsPerInterval$avg_steps=as.numeric(avgStepsPerInterval$avg_steps)

#draw the time series plot
plot(avgStepsPerInterval$interval, avgStepsPerInterval$avg_steps, type="l", xlab="interval", ylab="Average Steps", main="Average Steps Per Interval")

abline(h = max(avgStepsPerInterval$avg_steps), col = "royalblue", lwd = 2)

legend(x = "topright", c("maximum"),  col=c("royalblue"), lwd = c(2) )
text(0,200,round(max(avgStepsPerInterval$avg_steps)))
