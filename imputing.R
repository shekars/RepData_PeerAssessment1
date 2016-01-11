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

avgStepsPerInterval$avg_steps=as.double(avgStepsPerInterval$avg_steps)

#get the list of records having missing values
naActivity <- activity[is.na(activity$steps)==TRUE,]

#replace missing values with  avg no. of steps for that interval
naActivity$steps<-lapply(naActivity$interval, function(x) avgStepsPerInterval[avgStepsPerInterval$interval==x,]$avg_steps)

naActivity$steps=as.double(naActivity$steps)

#combine missing values dataset
newDataSet <- rbind(activity[is.na(activity$steps)==FALSE,], naActivity)
newDataSet$steps=as.double(newDataSet$steps)

#group by date
dateGroupByActivity <- group_by(newDataSet,  date)

#calculate total steps per date
stepsPerDay <- summarize(dateGroupByActivity, total_steps=sum(as.numeric(steps)))

#drow the histogram
hist(stepsPerDay$total_steps, xlab="Steps Per Day", main="Histogram of steps per day", col="grey")

#display the mean
abline(v = mean(stepsPerDay$total_steps), col = "royalblue", lwd = 2)
text(mean(na.omit(stepsPerDay$total_steps)),20, round(mean(na.omit(stepsPerDay$total_steps))))

#display the median
abline(v = median(stepsPerDay$total_steps), col = "red", lwd = 2)
text(median(na.omit(stepsPerDay$total_steps)),25,round(median(na.omit(stepsPerDay$total_steps))))

#draw the legend
legend(x = "topright", c("Mean", "Median"),  col = c( "royalblue", "red"), lwd = c(2, 2))

