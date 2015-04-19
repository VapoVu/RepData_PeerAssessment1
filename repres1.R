# Reproducible research
# Assignment 1

library(plyr, lattice)

# Loading and preprocessing the data

pvDat = read.csv('activity.csv')

pvu <- aggregate(steps ~ date, data = pvDat, sum, na.rm =T)

summary(pvu)

# What is mean total number of steps taken per day?


hist(pvu$steps, breaks =10)

ss = c(mean(pvu$steps), median(pvu$steps))

# What is the average daily activity pattern?

pvi <- aggregate(steps ~ interval, data = pvDat, mean, na.rm =T)
pvm <- aggregate(steps ~ interval, data = pvDat, median, na.rm =T)

names(pvm) <- c("interval", "medStep")

names(pvi) <- c("interval", "medStep")

plot(pvi$interval, pvi$steps, xlab = '5-minute interval', 
     ylab = 'Daily Average Steps', type = "l")


ind = which(pvi$steps == max(pvi$steps))

# Imputing missing values

nmiss <- colSums(is.na(pvDat))


pvJoin <- join(pvDat, pvm, by = "interval", type = "left", match = "all")

pvJoin$steps[is.na(pvJoin$steps)] <- pvJoin$medStep[is.na(pvJoin$steps)]

pvFill <- pvJoin[,c(1:3)]

pvs <- aggregate(steps ~ date, data = pvFill, sum, na.rm =T)

hist(pvs$steps, breaks =10)

ss0 = c(mean(pvs$steps), median(pvs$steps))


# Are there differences in activity patterns between weekdays and weekends?

pvDat$weekday <- weekdays(as.Date(pvDat$date))

pvDat$wkend <- "weekday"
pvDat$wkend[pvDat$weekday %in% c("Saturday", "Sunday")] = "weekend"

pvk <- aggregate(steps ~ interval, data = pvDat, mean, subset = (wkend == "weekend"),na.rm =T)

pvd <- aggregate(steps ~ interval, data = pvDat, mean, subset = (wkend == "weekday"),na.rm =T)

xyplot(steps ~ interval, xlab = '5-minute interval', 
     ylab = 'Daily Average Steps', type = "l")

xyplot(steps ~ interval, xlab = '5-minute interval', 
     ylab = 'Daily Average Steps', type = "l")
