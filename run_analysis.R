#1. Merges the training and the test sets to create one data set.

#read general data and extract column headings

features<-read.table("./features.txt",header=FALSE)
activityLabels<-read.table("./activity_labels.txt",header=FALSE)

#read Training data
subjectTrain<-read.table("./train/subject_train.txt", header=FALSE)
xTrain<-read.table("./train/X_train.txt", header=FALSE)
yTrain<-read.table("./train/y_train.txt", header=FALSE)

#Assign column names

colnames(activityLabels)<-c("activityId","activityType")
colnames(subjectTrain)<-"subId"
colnames(xTrain)<-features[,2]
colnames(yTrain)<-"activityId"

#Merging training Data...
trainData <- cbind(yTrain,subjectTrain,xTrain)

#Reading the test Data
subjectTest<-read.table("./test/subject_test.txt", header=FALSE)
xTest<-read.table("./test/X_test.txt", header=FALSE)
yTest<-read.table("./test/y_test.txt", header=FALSE)

# Assign column names.. same as for training data..
colnames(subjectTest) <- "subId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

# merging test Data
testData <- cbind(yTest,subjectTest,xTest)

#final merged data
finalData <- rbind(trainData,testData)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

data_mean_std <-finalData[,grepl("mean|std|sub|activityId",colnames(finalData))]

#3. Uses descriptive activity names to name the activities in the data set

library(plyr)
data_mean_std <- join(data_mean_std, activityLabels, by = "activityId", match = "first")
data_mean_std <-data_mean_std[,-1]

#4. Appropriately labels the data set with descriptive variable names.
data_mean_std<-rename(data_mean_std,Acc="Acceleration")
#Remove parentheses
names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl  = TRUE)
#correct syntax in names
names(data_mean_std) <- make.names(names(data_mean_std))
#add descriptive names
names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

#5 creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata_average_sub<- ddply(data_mean_std, c("subId","activityType"), numcolwise(mean))
write.table(tidydata_average_sub,file="tidydata.txt")