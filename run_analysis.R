##################################### Getting and Cleaning data assignment #################################

library(dplyr)
### Getting the data

file <- "Coursera_DS3_Final.zip"

# Checking if archieve already exists.
if (!file.exists(file)){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, file, method="curl")
}  

# Checking if folder exists
if (!file.exists("UCI HAR Dataset")) { 
  unzip(file) 
}

##### Reading the data

features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n","functions"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")

##### 1. merging the training and test data

df_X <- rbind(x_train, x_test)
df_Y <- rbind(y_train, y_test)
Subject <- rbind(subject_train, subject_test)
Merged_Data <- cbind(Subject, df_Y, df_X)

##### 2. Extracting only measurements on mean and sd

tidy_xy<- Merged_Data %>% select(subject, code, contains("mean"), contains("std"))


##### 3. Assigning descriptive activity names to name the activities in the data set

tidy_xy$code <- activities[tidy_xy$code, 2]

##### 4. Appropriately label data set with descriptive variable names

names(tidy_xy)[2] = "activity"
names(tidy_xy)<-gsub("Acc", "Accelerometer", names(tidy_xy))
names(tidy_xy)<-gsub("Gyro", "Gyroscope", names(tidy_xy))
names(tidy_xy)<-gsub("BodyBody", "Body", names(tidy_xy))
names(tidy_xy)<-gsub("Mag", "Magnitude", names(tidy_xy))
names(tidy_xy)<-gsub("^t", "Time", names(tidy_xy))
names(tidy_xy)<-gsub("^f", "Frequency", names(tidy_xy))
names(tidy_xy)<-gsub("tBody", "TimeBody", names(tidy_xy))
names(tidy_xy)<-gsub("-mean()", "Mean", names(tidy_xy), ignore.case = TRUE)
names(tidy_xy)<-gsub("-std()", "STD", names(tidy_xy), ignore.case = TRUE)
names(tidy_xy)<-gsub("-freq()", "Frequency", names(tidy_xy), ignore.case = TRUE)
names(tidy_xy)<-gsub("angle", "Angle", names(tidy_xy))
names(tidy_xy)<-gsub("gravity", "Gravity", names(tidy_xy))

##### 5. From tidy_xy creates a second, independent tidy data set with the average of each variable for each activity and each subject.

FinalData <- tidy_xy %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)
str(FinalData)


