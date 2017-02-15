library(dplyr)

temp <- tempfile()
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)

#1. Merges the training and the test sets to create one data set.


s.train <- read.table( unz(temp, filename = "UCI HAR Dataset/train/subject_train.txt") )
x.train <- read.table( unz(temp, filename = "UCI HAR Dataset/train/X_train.txt") )
y.train <- read.table( unz(temp, filename = "UCI HAR Dataset/train/y_train.txt") )

s.test <- read.table( unz(temp, filename = "UCI HAR Dataset/test/subject_test.txt") )
x.test <- read.table( unz(temp, filename = "UCI HAR Dataset/test/X_test.txt") )
y.test <- read.table( unz(temp, filename = "UCI HAR Dataset/test/y_test.txt") )

features <- read.table ( unz(temp, filename = "UCI HAR Dataset/features.txt") )[,2]
activity_labels <- read.table ( unz(temp, filename = "UCI HAR Dataset/activity_labels.txt") )

traindf <- data.frame(s.train, y.train, x.train)
testdf <- data.frame(s.test, y.test, x.test)

df <- rbind(traindf, testdf)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

features <- make.names(features, unique = T)
names(df) <- c("subject", "activity", features)

newdf <- select(df, matches("mean|std|activity|subject"))

#3. Uses descriptive activity names to name the activities in the data set
names(activity_labels) <- c("activity", "activitylabel")
newdf <- merge(x = activity_labels, y = newdf, by = "activity")
newdf <- select(newdf, -activity)
newdf <- select(newdf, subject, activitylabel, 3:88)
newdf <- arrange(newdf, subject)

#4. Appropriately labels the data set with descriptive variable names.
names(newdf) <- tolower(names(newdf)) #make all variable names lowercase

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydf <- newdf %>% group_by(subject, activitylabel) %>% summarise_each(funs(mean(., na.rm =T)))

write.table(tidydf, file = "~/tidy.txt", row.name=FALSE)

