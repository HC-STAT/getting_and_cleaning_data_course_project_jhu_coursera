library(dplyr)

# download the data set

if (!file.exists("dataset.zip")){
    fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileUrl, "dataset.zip")
}
if (!file.exists("UCI HAR Dataset")){
    unzip("dataset.zip")
}


# load the data set
acativityLabels <- read.table("UCI HAR Dataset/activity_labels.txt")
features <- read.table("UCI HAR Dataset/features.txt")

subjectTrain <- read.table('UCI HAR Dataset/train/subject_train.txt')
xTrain <- read.table('UCI HAR Dataset/train/x_train.txt')
names(xTrain) <- features[, 2]
yTrain <- read.table('UCI HAR Dataset/train/y_train.txt')

subjectTest <- read.table('UCI HAR Dataset/test/subject_test.txt')
xTest <- read.table('UCI HAR Dataset/test/x_test.txt')
names(xTest) <- features[, 2]
yTest <- read.table('UCI HAR Dataset/test/y_test.txt')


# Merges the training and the test sets to create one data set
subjectDataSet <- rbind(subjectTrain, subjectTest)
xDataSet <- rbind(xTrain, xTest)
yDataSet <- rbind(yTrain, yTest)


# Extracts only the measurements on the mean and standard deviation for each measurement
xDataSet_Mean_Std <- xDataSet[, grep("-(mean|std)", features[, 2])]


# Uses descriptive activity names to name the activities in the data set
yDataSet[, 1] <- acativityLabels[yDataSet[, 1], 2]
names(yDataSet) <- "activity"


# Appropriately labels the data set with descriptive variable names
names(subjectDataSet) <- "subject"
summary(subjectDataSet)

DataSet <- cbind(subjectDataSet, yDataSet, xDataSet_Mean_Std)
names(DataSet) <- gsub("Acc","Acceleration",names(DataSet))
names(DataSet) <- gsub("Mag","Magnitude",names(DataSet))
names(DataSet) <- gsub("^t","Time",names(DataSet))
names(DataSet) <- gsub("^f","Frequency",names(DataSet))
names(DataSet) <- gsub("-mean\\(\\)-","Mean",names(DataSet))
names(DataSet) <- gsub("-std\\(\\)-","StandardDeviation",names(DataSet))
names(DataSet) <- gsub("mean","Mean",names(DataSet))
names(DataSet) <- gsub("std","StandardDeviation",names(DataSet))
names(DataSet) <- gsub("-","",names(DataSet))
names(DataSet) <- gsub("\\(\\)","",names(DataSet))


# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidyDataSet <- DataSet %>% 
    group_by(subject, activity) %>%
    summarise_all(funs(mean))

write.table(tidyDataSet, "tidydata.txt", row.name=FALSE)

