# Week 4 Project
# Getting and Cleaning Data, Coursera
#
# Filename: run_analysis.R
#
# Project description: 
# You should create one R script called run_analysis.R that does the following. 
# 1.Merges the training and the test sets to create one data set.
# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.Uses descriptive activity names to name the activities in the data set
# 4.Appropriately labels the data set with descriptive variable names. 
# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#
#
#
library(dplyr)
#
# STEP 0, Part 1: Get the data.
# Download the zip file. 
zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile, mode = "wb")
}

# Unzip the zip file containing the data if the data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}
#
#
# STEP 0, Part 2: Read the training data.
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))
#
# Read the test data.
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))
#
# Read the features; don't convert text labels to factors.
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)
#
# Read the activity labels.
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")
#
#
#
# Step 1. Merge the training and the test sets to create one data set.
# concatenate individual data tables to make single data table
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)
#
# Remove the individual data tables in order to save memory.
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)
#
# Assign column names.
colnames(humanActivity) <- c("subject", features[, 2], "activity")
#
#
# Step 2. Extract only measurements on the mean and standard deviation for each measurement.
# Determine columns of data set to keep based on column name.
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
# Keep data in columns.
humanActivity <- humanActivity[, columnsToKeep]
#
#
# Step 3. Assign descriptive activity names to name the activities in the dataset.
# Replace all activity values with named factor levels.
humanActivity$activity <- factor(humanActivity$activity, 
                                 levels = activities[, 1], labels = activities[, 2])
#
#
# Step 4. Label the dataset with descriptive variable names.
# Get the column names.
humanActivityCols <- colnames(humanActivity)
# Remove any special characters.
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)
# Expand the abbreviations and clean up all names.
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)
# Correct typos.
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)
# Re-label as a column of names.
colnames(humanActivity) <- humanActivityCols
#
#
# Step 5. Create a second, independent tidy set with the average of each variable for each activity and each subject.
# Group by subject & activity, then summarize using means.
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarize_each(funs(mean))
# Output all results to a file named "tidydata.txt".
write.table(humanActivityMeans, "tidydata.txt", row.names = FALSE, quote = FALSE)