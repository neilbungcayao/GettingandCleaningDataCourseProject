library(data.table)
library(dplyr)
library(reshape2)

path <- getwd()

file_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file_url, "data.zip")
unzip(zipfile = "data.zip")


# Load activity labels + features
activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames]) #filter only means and stds
measurements <- features[featuresWanted, featureNames]
measurements <- gsub('[()]', '', measurements) #replace parentheses


# Load train datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)


# Load test datasets
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
setnames(test, colnames(test), measurements)
testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)


# rbinding the train and test datasets (merging)
full_dataset <- rbind(train, test)


# replacing the descriptive activity class labels to descriptive names in the data set
full_dataset$Activity <- as.factor(full_dataset$Activity)
full_dataset$Activity <- ifelse(full_dataset$Activity == 1, "WALKING",
                         ifelse(full_dataset$Activity == 2, "WALKING_UPSTAIRS",
                         ifelse(full_dataset$Activity == 3, "WALKING_DOWNSTAIRS",
                         ifelse(full_dataset$Activity == 4, "SITTING",
                         ifelse(full_dataset$Activity == 5, "STANDING", "LAYING")))))

# appropriately label the dataset with descriptive variables and creating data set 
# with the average of each variable for each activity and each subject
full_dataset$SubjectNum <- as.factor(full_dataset$SubjectNum)
full_dataset <- melt(data = full_dataset, id = c("SubjectNum", "Activity"))
full_dataset <- dcast(data = full_dataset, SubjectNum + Activity ~ variable, fun.aggregate = mean)

# exporting the tidy data as a text file
fwrite(x = full_dataset, file = "tidydata.txt", quote = FALSE)
