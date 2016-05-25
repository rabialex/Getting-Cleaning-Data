##1. Merges the training and the test sets to create one data set.
##2. Extracts only the measurements on the mean and standard deviation for each measurement.
##3. Uses descriptive activity names to name the activities in the data set
##4. Appropriately labels the data set with descriptive variable names.
##5. From the data set in step 4, creates a second, independent tidy data set 
##   with the average of each variable for each activity and each subject.
library(data.table)


## Training Data
trainSet <- read.table("../getandcleaningData/week4/UCI HAR Dataset/train/X_train.txt")
trainActivity <- read.table("../getandcleaningData/week4/UCI HAR Dataset/train/y_train.txt")
trainSubject <- read.table("../getandcleaningData/week4/UCI HAR Dataset/train/subject_train.txt")

## Test Data
testSet <- read.table("../getandcleaningData/week4/UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("../getandcleaningData/week4/UCI HAR Dataset/test/y_test.txt")
testSubject <- read.table("../getandcleaningData/week4/UCI HAR Dataset/test/subject_test.txt")

## Activity and Feature Data
## ActivityLabels list the activty to number value for Activity tables
ActivityLabels <- read.table("../getandcleaningData/week4/UCI HAR Dataset/activity_labels.txt")
FeaturesColumnNames <- read.table("../getandcleaningData/week4/UCI HAR Dataset/features.txt")

## Combined test and training data 
combinedSet <- rbind(testSet,trainSet)
combinedActivity <- rbind(testActivity, trainActivity)
combinedSubject <- rbind(testSubject, trainSubject)

## Add Variable names to Combined data tables
colnames(combinedSet) <- t(FeaturesColumnNames[2])
colnames(combinedActivity) <- "Activity"
colnames(combinedSubject) <- "Subject"

## Part 3: Use descriptive names for Activities in the combinedActivity data table
for(i in 1:6){
  combinedActivity[combinedActivity == i] <- as.character(ActivityLabels[i,2])
}

## Part 1: Merge data final dimensions 10299 observations by 563 variables
CompleteDT <- cbind(combinedSet,combinedActivity,combinedSubject)

## Part 2: Extract only variables on the mean and standard deviation
ExtractedData <- CompleteDT[grep("mean|std", colnames(CompleteDT), ignore.case = TRUE)]
ExtractedData <- cbind(ExtractedData, combinedActivity, combinedSubject)

## Part 4: Use Descriptive Names for Variables in ExtractedData
names(ExtractedData) <- gsub("Acc","acceleration",names(ExtractedData))
names(ExtractedData) <- gsub("Gyro","gyroscope",names(ExtractedData))
names(ExtractedData) <- gsub("Gravity","gravity",names(ExtractedData))
names(ExtractedData) <- gsub("BodyBody","body",names(ExtractedData))
names(ExtractedData) <- gsub("Body", "body", names(ExtractedData))
names(ExtractedData) <- gsub("Mag","magnitude",names(ExtractedData))
names(ExtractedData) <- gsub("^t","time",names(ExtractedData))
names(ExtractedData) <- gsub("^f","frequency",names(ExtractedData))
names(ExtractedData) <- gsub("Freq", "frequency",names(ExtractedData))
names(ExtractedData) <- gsub("tBody","timebody",names(ExtractedData))
names(ExtractedData) <- gsub("tbody","timebody",names(ExtractedData))
names(ExtractedData) <- gsub("Mean","mean",names(ExtractedData))
names(ExtractedData) <- gsub("std","std",names(ExtractedData))
names(ExtractedData) <- gsub("Jerk","jerk",names(ExtractedData))
names(ExtractedData) <- gsub("-","",names(ExtractedData))

## Part 5: Create new tidy data set from ExtractedData. Organize such that 
## the average of each variable (columns) for each activity and each subject.

tidyData <- aggregate(.~Subject + Activity, ExtractedData,mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]

write.table(tidyData, file = "tidy_data.txt", row.names = FALSE)
