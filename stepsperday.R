library(dplyr)

#read the activity.csv file
activity <- read.csv2("activity.csv", sep=",", na.strings = "NA")

#define the data type of the fields
activity$steps <- as.numeric(activity$steps)
activity$interval <- as.numeric(activity$interval)
activity$date <- as.Date(activity$date)

#group by data
dateGroupByActivity <- group_by(activity,  date)

#calculate the total steps using summarize function
stepsPerDay <- summarize(dateGroupByActivity, total_steps=sum(na.omit(steps)))

#show the histor gram
hist(na.omit(stepsPerDay$total_steps), xlab="Steps Per Day", main="Histogram of steps per day", col="grey")

#display the mean
abline(v = mean(na.omit(stepsPerDay$total_steps)), col = "royalblue", lwd = 2)
text(mean(na.omit(stepsPerDay$total_steps)),20, round(mean(na.omit(stepsPerDay$total_steps))))

#display the median
abline(v = median(na.omit(stepsPerDay$total_steps)), col = "red", lwd = 2)
text(median(na.omit(stepsPerDay$total_steps)),25,median(na.omit(stepsPerDay$total_steps)))
legend(x = "topright", c("Mean", "Median"),  col = c( "royalblue", "red"), lwd = c(2, 2))

