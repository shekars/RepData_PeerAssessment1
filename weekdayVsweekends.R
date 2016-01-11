library(dplyr)

#read the activity.csv file
activity <- read.csv2("activity.csv", sep=",", na.strings = "NA")

#define the data type of the fields
activity$steps <- as.double(activity$steps)
activity$interval <- as.numeric(activity$interval)
activity$date <- as.Date(activity$date)

#group by interval
intervalGroupByActivity <- group_by(activity,  interval)

#calulate avg no. of steps
avgStepsPerInterval <- summarize(intervalGroupByActivity, avg_steps=mean(na.omit(steps)))

#get the list of records having missing values
naActivity <- activity[is.na(activity$steps)==TRUE,]

#replace missing values with  avg no. of steps for that interval
naActivity$steps<-lapply(naActivity$interval, function(x) avgStepsPerInterval[avgStepsPerInterval$interval==x,]$avg_steps)
naActivity$steps=as.double(naActivity$steps)

#combine missing values dataset
newDataSet <- rbind(activity[is.na(activity$steps)==FALSE,], newActivity)

#defne week days
week_days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

#define the factor variable
newDataSet$dayType  <- factor((weekdays(newDataSet$date) %in% week_days), levels=c(FALSE,TRUE), labels=c('weekend','weekday'))

#group by day Type and interval
intervalGroupByActivity <- group_by(newDataSet,  dayType, interval)

#calculate the average no. of steps
newSummary <- summarize(intervalGroupByActivity,avg_steps=mean(steps))

png(file="figures/weekend_vs_weekday.png", width=480, height=480)

#draw the plot
par(mfrow=c(2,1))

weekdaySummary <- newSummary[newSummary$dayType=='weekday',]
weekendSummary <- newSummary[newSummary$dayType=='weekend',]
  
plot(weekdaySummary$interval, weekdaySummary$avg_steps, type="l", xlab="interval", ylab="Steps", main="Weekday")
plot(weekendSummary$interval, weekendSummary$avg_steps, type="l", xlab="interval", ylab="Steps", main="Weekend")

dev.off()