library(dplyr)

if (!file.exists("data")) {
  dir.create("data")
}

#if we don't have a copy of the dataset locally, download one
if (!file.exists("data/UCI HAR Dataset")) {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, destfile = "./data/uci_har.zip", method = "curl")
  unzip("./data/uci_har.zip", exdir = "./data")
}

features <- read.table("data/UCI HAR Dataset/features.txt")[, 2]
activity_names <- read.table("data/UCI HAR Dataset/activity_labels.txt")

#read in both the test data set and the training data sets for the x observations
train_x <- read.table("data/UCI HAR Dataset/train/X_train.txt")
test_x <- read.table("data/UCI HAR Dataset/test/X_test.txt")
x_merged <- rbind(train_x, test_x)

# Filter down to only the columns containing mean or std
matches <- grep("(mean|std)\\(\\)", features)
filtered <- x_merged[, matches]
names(filtered) <- features[matches]

#cleanup the column names removing hyphens, and normalizing fields
names(filtered) <- gsub("^t", "Time", names(filtered))
names(filtered) <- gsub("^f", "Frequency", names(filtered))
names(filtered) <- gsub("mean\\(\\)", "Mean", names(filtered))
names(filtered) <- gsub("std\\(\\)", "Deviation", names(filtered))
names(filtered) <- gsub("-", "", names(filtered))

#merge the two y dataframes into one frame
train_y<- read.table("data/UCI HAR Dataset/train/y_train.txt")
test_y <- read.table("data/UCI HAR Dataset/test/y_test.txt")
y_merged <- rbind(train_y, test_y)

#join to the activity names to fill out meaningful names for each recorded activity
Activity <- left_join(y_merged, activity_names, by = "V1")[,2]
names(Activity) <- c("Activity")

#merge the subject data into a single dataframe
train_subject <- read.table("data/UCI HAR Dataset/train/subject_train.txt")
test_subject <- read.table("data/UCI HAR Dataset/test/subject_test.txt")
subject_merged <- rbind(train_subject, test_subject)
names(subject_merged) <- c("Subject")

#merge all the partial frames into one complete data frame
full_frame <- cbind(subject_merged, Activity, filtered)

#Tidy - group the data set by subject and activity, then generate the mean for each observation per group
by_subject <- group_by(full_frame, Subject, Activity) %>% summarise_each(funs(mean))

#write out the tidy table
write.table(by_subject, "data/tidy.txt", row.names = FALSE)