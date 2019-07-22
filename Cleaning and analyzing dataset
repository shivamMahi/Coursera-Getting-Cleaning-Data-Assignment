# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, outputting the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.
#

library(dplyr)
install.packages("reshape2")
library(reshape2)

                         #####################
                          # STEP 0A - Get data
                         #####################

# download zip file containing data if it hasn't already been downloaded

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipUrl, zipFile,mode = "wb")
}

# unzip zip file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

                            #########################
                            # STEP 0B - Read data
                            #########################

# read training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# read test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

#Read the features data
features = read.table(file.path(dataPath, "features.txt"),header = FALSE)
#Read activity labels data
activityLabels = read.table(file.path(dataPath, "activity_labels.txt"),header = FALSE)

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

train <- cbind(trainingSubjects,trainingValues,trainingActivity)
test <- cbind(testSubjects, testValues, testActivity)

allData <- rbind(train, test)

colnames(allData) <- c("subject", "values", "activities")

head(allData,5)


                ##########################################################
                # Step 2 - Extract feature cols & names named 'mean, std'
                ##########################################################

columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivityCols))

# ... and keep data in these columns only
humanActivityCols <- humanActivityCols[, columnsToKeep]

humanActivityCols


##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data set
##############################################################################
setWithActivityNames = merge(setForMeanAndStd, activityLabels, by='activityId', all.x=TRUE)


##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# get column names
humanActivityCols <- colnames(humanActivity)

# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

humanActivityCols


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)


