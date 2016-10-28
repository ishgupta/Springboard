library(dplyr)
library(tidyr)
library(data.table)

features <- readLines("UCI HAR Dataset/features.txt")
head(features)
tail(features)


activities <- read.table("UCI HAR Dataset/activity_labels.txt", row.names = NULL)
dim(activities)
names(activities) <- c("activityId", "activity")
activities <- spread(activities, activityId, activity)
activities <- as.character(activities)
glimpse(activities)

features <- read.table("UCI HAR Dataset/features.txt")
dim(features)
names(features) <- c("featureId", "feature")
glimpse(features)


train_data <- readLines("UCI HAR Dataset/train/X_train.txt")
train_data <- gsub("  ", " ", train_data)
cat(train_data, file = "UCI HAR Dataset/train/X_train_clean.txt", sep = "\n")

train_x <- read.table("UCI HAR Dataset/train/X_train_clean.txt", sep = " ", row.names = NULL)
dim(train_x)

test_data <- readLines("UCI HAR Dataset/test/X_test.txt")
test_data <- gsub("  ", " ", test_data)
cat(test_data, file = "UCI HAR Dataset/test/X_test_clean.txt", sep = "\n")

test_x <- read.table("UCI HAR Dataset/test/X_test_clean.txt", sep = " ", row.names = NULL)
dim(test_x)



sum(!is.na(train_x[, 1]))
train_x <- train_x[, -1]

featureNames <- make.names(features$feature)

names(train_x) <- featureNames
dim(train_x)

train_activities <- read.table("UCI HAR Dataset/train/y_train.txt")
names(train_activities) <- "activityId"
dim(train_activities)

activities_list <- c("1" = "WALKING", "2" = "WALKING_UPSTAIRS", "3" = "WALKING_DOWNSTAIRS", "4" = "SITTING", "5" = "STANDING", "6" = "LAYING")

train_activities$activity <- activities_list[train_activities$activityId]
dim(train_activities)

train_x$ActivityLabel <- train_activities$activityId
train_x$ActivityName <- train_activities$activity
dim(train_x)


subjects_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
table(subjects_train)

train_x$subject <- subjects_train$V1
dim(train_x)

glimpse(train_x)



dim(test_x)
sum(!is.na(test_x[, 1]))
test_x <- test_x[, -1]

names(test_x) <- featureNames
dim(test_x)

test_activities <- read.table("UCI HAR Dataset/test/y_test.txt")
dim(test_activities)
names(test_activities) <- "activityId"
test_activities$activity <- activities_list[test_activities$activityId]
dim(test_activities)

test_x$ActivityLabel <- test_activities$activityId
test_x$ActivityName <- test_activities$activity
dim(test_x)

subjects_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
dim(subjects_test)

test_x$subject <- subjects_test$V1


setdiff(names(train_x), names(test_x))

row_names <- rownames(test_x)
row_names <- as.character(as.integer(row_names) + length(rownames(train_x)))
rownames(test_x) <- row_names

#complete_data <- bind_rows(train_x, test_x)
library(gtools)
complete_data <- smartbind(train_x, test_x)

dim(complete_data)

complete_data %>% group_by(subject, ActivityLabel) %>%  summarise_each(1:561, funs = ("mean"))

complete_data[duplicated(names(complete_data))]


requiredCols <- names(complete_data)[!names(complete_data) %in% c("ActivityLabel", "ActivityName", "subject")]

requiredColIndexes <- which(!names(complete_data) %in% c("ActivityLabel", "ActivityName", "subject"))

newNames <- paste("avg_", requiredCols)

tidy_data <- complete_data  %>% group_by(subject, ActivityLabel, ActivityName)  %>% summarise_each(requiredColIndexes, funs = c("mean"), vars= requiredColIndexes)

requiredCols_string <- paste("avg_", requiredCols)
requiredCols_string = append(requiredCols_string, names(tidy_data)[1:3], after = 0)
names(tidy_data) <- requiredCols_string

write_csv(tidy_data, "tidy.csv")
