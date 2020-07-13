setwd("D:/Amr/Data Science/Getting and Cleaning Data/Project")
library(dplyr)
## Read online file
FileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
##download the file
download.file(FileURL, "./UCI-HAR-dataset.zip", method="auto")
##unzip and create directory automatically
unzip("./UCI-HAR-dataset.zip")


## 1. Merges the training and the test sets to create one data set
TrainingData<-read.table("./UCI HAR Dataset/train/X_train.txt", sep = "")
TestingData<-read.table("./UCI HAR Dataset/test/X_test.txt", sep = "")
# Read features and activities vector
Features <- read.table("./UCI HAR Dataset/features.txt", as.is = TRUE, stringsAsFactors = FALSE)[2]
Activities <- read.table("./UCI HAR Dataset/activity_labels.txt")[2]
# Merge testing and training data (Answer to question 1)
MergedSet<- rbind(TrainingData, TestingData)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement
# Get indices of mean and std columns
MeanStd <- grep("mean|std", Features$V2)
colnames(MergedSet) <- Features$V2
MeanStdColumn <- MergedSet[, MeanStd]


# 3. Uses descriptive activity names to name the activities in the data set
# Read Activities
TrainingActivities <- read.table("./UCI HAR Dataset/train/y_train.txt", sep = "")
TestingActivities<-read.table("./UCI HAR Dataset/test/y_test.txt", sep = "")
MergedActivities <- rbind(TrainingActivities, TestingActivities)
rm(TrainingActivities, TestingActivities, TrainingData, TestingData, FileURL)    # Clean up
# Create function to replace index with name of activity
ReplaceActivity<-function(ActivityID)
{
    Activities$V2[ActivityID]
}
ActivityName <- lapply(MergedActivities, ReplaceActivity)
ActivityName <- ActivityName$V1


# 4. Appropriately labels the data set with descriptive variable names. 
MergedSet <- cbind(ActivityName, MergedSet)
MeanStdColumn <- cbind(ActivityName, MeanStdColumn)

# Read PersonID (subjects)
TestSubjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")
TrainingSubjects<- read.table("./UCI HAR Dataset/train/subject_train.txt")
Volunteers <- rbind(TrainingSubjects, TestSubjects)
colnames(Volunteers) <- "VolunteerID"
rm(TestSubjects, TrainingSubjects)

# 5. From the data set in step 4, creates a second, independent tidy data set with 
# the average of each variable for each activity and each subject
MeanStdColumn <- cbind(Volunteers, MeanStdColumn)   # Create a tidy data of the MeanStdColumn dataframe

GroupedSet <- group_by(MeanStdColumn, ActivityName, VolunteerID)
FinalResult <- GroupedSet %>% 
    summarize(across(everything(), mean))
