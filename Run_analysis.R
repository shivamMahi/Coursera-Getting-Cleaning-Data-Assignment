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

featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

                            #########################
                            # STEP 0B - Read data
                            #########################

# read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# read test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
#Read the features data
features = read.table(file.path(dataPath, "features.txt"),header = FALSE)
#Read activity labels data
activityLabels = read.table(file.path(dataPath, "activity_labels.txt"),header = FALSE)

##############################################################################
# Step 1 - Merge the training and the test sets to create one data set
##############################################################################

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

colnames(features) <- t(featureNames[2])

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

head(allData,5)


                ##########################################################
                # Step 2 - Extract feature cols & names named 'mean, std'
                ##########################################################

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)

extractedData <- completeData[,requiredColumns]
dim(extractedData)

##############################################################################
# Step 3 - Use descriptive activity names to name the activities in the data set
##############################################################################
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}

extractedData$Activity <- as.factor(extractedData$Activity)

##############################################################################
# Step 4 - Appropriately label the data set with descriptive variable names
##############################################################################

names(extractedData)

names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

names(extractedData)


##############################################################################
# Step 5 - Create a second, independent tidy set with the average of each
#          variable for each activity and each subject
##############################################################################

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

# group by subject and activity and summarise using mean
humanActivityMeans <- extractedData %>% 
  group_by(Subject, Activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)


