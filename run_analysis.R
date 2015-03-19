# The source of data for this project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

#Set working directory
setwd("C:/DataScience/Getting-and-Cleaning-Data")

# Step1. Merges the training and the test sets
trainData <- read.table("./data/train/X_train.txt")
dim(trainData)
head(trainData)
trainLabel <- read.table("./data/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./data/train/subject_train.txt")
testData <- read.table("./data/test/X_test.txt")
dim(testData)
testLabel <- read.table("./data/test/y_test.txt")
table(testLabel)
testSubject <- read.table("./data/test/subject_test.txt")
MergeData <- rbind(trainData, testData)
dim(MergeData)
MergeLabel <- rbind(trainLabel, testLabel)
dim(MergeLabel) 
MergeSubject <- rbind(trainSubject, testSubject)
dim(MergeSubject)

# Step2. Extracts only the mean and standard measurements
# deviation for each measurement.
features <- read.table("./data/features.txt")
dim(features) 
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
MergeData <- MergeData[, meanStdIndices]
dim(MergeData) 
names(MergeData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(MergeData) <- gsub("mean", "Mean", names(MergeData)) # capitalize M
names(MergeData) <- gsub("std", "Std", names(MergeData)) # capitalize S
names(MergeData) <- gsub("-", "", names(MergeData)) # remove "-" in column names

# Step3. Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[MergeLabel[, 1], 2]
MergeLabel[, 1] <- activityLabel
names(MergeLabel) <- "activity"

# Step4. Appropriately labels the data set with descriptive activity names.

names(MergeSubject) <- "subject"
cleanedData <- cbind(MergeSubject, MergeLabel, MergeData)
dim(cleanedData) # 10299*68
write.table(cleanedData, "merged_data.txt", ,row.names = FALSE) # write out the 1st dataset

# Step5. Creates a second, independent tidy data set with the 
# average of each variable for each activity and each subject.

subjectLen <- length(table(MergeSubject)) # 30
activityLen <- dim(activity)[1] # 6
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(i in 1:subjectLen) {
  for(j in 1:activityLen) {
    result[row, 1] <- sort(unique(MergeSubject)[, 1])[i]
    result[row, 2] <- activity[j, 2]
    bool1 <- i == cleanedData$subject
    bool2 <- activity[j, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}

head(result)
write.table(result, "data_with_means.txt",row.names = FALSE) # Th 2nd dataset required