# read the descriptive files
activityLabels <- as.matrix(read.table("./UCI HAR Dataset/activity_labels.txt"))
features <- read.table("./UCI HAR Dataset/features.txt")
# read the training files
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
# read the test files
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt")

# generate and label training and test set with descriptive variable names (4th in the list)
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "ActivityLabels"
myTrain<-cbind(subjectTrain, yTrain, xTrain)

colnames(xTest) <- features[,2]
colnames(yTest) <- "ActivityLabels"
myTest<-cbind(subjectTest, yTest, xTest)

# Merge the training and the test sets to create one data set.
myTT <- rbind(myTrain, myTest)
colnames(myTT)[1] <- "Subject"

# Extract only the measurements on the mean and standard deviation for each measurement. 
subsetMean <- myTT[,grepl("mean",colnames(myTT))]
subsetSTD <- myTT[,grepl("std",colnames(myTT))]

# Use descriptive activity names to name the activities in the data set
myMST <- cbind(myTT[,1:2],subsetMean, subsetSTD)
for(i in 1:6){
	myMST[grepl(i, myMST$ActivityLabels),2] <- activityLabels[i,2]
}

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
## make a subset for each subject (there are 30subjects in total) 
## and take the average of each variable for each activity, 
## then store it in the new dataset

newVec <- vector()
for(i in 1:30){
	for(j in 1:6){
		subset <- data.frame(myMST[(myMST$Subject==i & myMST$ActivityLabels==activityLabels[j,2]),])
		subset2 <- as.matrix(data.frame(subset[,3:81]))
		subset3 <- colMeans(subset2)
		subset4 <- cbind(i,activityLabels[j,2], rownames(t(t(subset3))), t(t(subset3)))
		rownames(subset4) <- NULL
		newVec <- rbind(newVec, subset4)
	}
}
colnames(newVec) <- c("Subject", "ActivityLabels", "TypeOfMeasurement", "AverageOfMeasurement")
newDF <- data.frame(newVec)
write.table(newDF, "tidyDataSet.txt", row.name=FALSE)




