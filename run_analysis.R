# OVERVIEW
#   Using data collected from the accelerometers from the Samsung Galaxy S 
#   smartphone, work with the data and make a clean data set, writing the
#   resulting tidy data to a file named "tidy_data.txt".
#   See README.md for details.

library(dplyr)
#download the zip file from provided link as below.
zipfile <-download("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",dest="./GettingCleaningData/Dataset.zip")
unzip(zipfile="./Dataset.zip",exdir="./UCI HAR Dataset")


#getting and cleaning training data from X_train.txt,subject_train.txt,y_train.txt

xtraindf<-read.table("./UCI HAR Dataset/train/X_train.txt",header=FALSE)#reads text file and loads into df
ytraindf<-read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
subject_train<-read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)


#getting and cleaning testing data from X_test.txt,subject_test.txt,y_test.txt


xtestdf<-read.table("./UCI HAR Dataset/test/X_test.txt",header=FALSE)
ytestdf<-read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
subject_test<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)

#Reading features.txt and loading into features dataframe and activity_labels.txt
features<-read.table("./UCI HAR Dataset/features.txt")
activityLabels<-read.table("./UCI HAR Dataset/activity_labels.txt")


#Assigning variable names(column names) to xtraindf,ytraindf,xtestdf,ytestdf,subject_train,subject_test,activityLabels using fatures.txt

colnames(xtraindf)<-features[,2]#Assigning column names for xtraindf
colnames(ytraindf)<-"ActivityId"
colnames(subject_train)<-"SubjectId"

colnames(xtestdf)<-features[,2]
colnames(ytestdf)<-"ActivityId"
colnames(subject_test)<-"SubjectId"

colnames(activityLabels)<-c("ActivityId","ActivityName")


#1.Merges the training and the test sets to create one data set.

train_set<-cbind(ytraindf,subject_train,xtraindf)#combines the data column wise
test_set<-cbind(ytestdf,subject_test,xtestdf)
mergered_set<-rbind(train_set,test_set)#combines the data row wise

#2.Extracts only the measurements on the mean and standard deviation for each measurement.
colNames<-colnames(mergered_set)
mean_and_std = (grepl("ActivityId" , colNames) | grepl("SubjectId" , colNames) | grepl("mean.." , colNames) | grepl("std.." , colNames))#gives logical dataframe for columns which has these conditions
mean_std_data<-mergered_set[,mean_and_std==TRUE]

#3.Uses descriptive activity names to name the activities in the data set

mean_std_data_with_names = merge(mean_std_data, activityLabels, by='ActivityId', all.x=TRUE)

#4.Appropriately labels the data set with descriptive variable names.

mean_std_data
mergered_set

#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

TidyDataSet <- aggregate(. ~SubjectId + ActivityId, mean_std_data_with_names, mean)
TidyDataSet <- TidyDataSet[order(TidyDataSet$SubjectId, TidyDataSet$ActivityId),]
TidyDataSet$ActivityId<-factor(TidyDataSet$ActivityId,levels = c(1,2,3,4,5,6),labels = c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYING'))

#write tidy_data to file
write.table(TidyDataSet, "tidy_data.txt", row.name=FALSE)
