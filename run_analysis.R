## Sergio Santos 2014-May-Coursera Getting and Cleaning Data

################################################################
#################################################################
##### START libraries 

library(plyr)
library(reshape)
library(reshape2)
library(plyr)
library(data.table)
################################################################
#################################################################
##### Read Files 

con_X_test<-file("./UCI HAR Dataset/test/X_test.txt", "r") 
file_X_test<-read.table(con_X_test)
close(con_X_test)


con_X_train<-file("./UCI HAR Dataset/train/X_train.txt", "r") 
file_X_train<-read.table(con_X_train)
close(con_X_train)

### features  list file ###############

con_features<-file("./UCI HAR Dataset/features.txt", "r") 
features_list<-read.table(con_features, header=FALSE)
close(con_features)


##### read activites column ###################################

con_y_train<-file("./UCI HAR Dataset/train/y_train.txt", "r") 
con_y_test<-file("./UCI HAR Dataset/test/y_test.txt", "r") 
activities_test<-read.table(con_y_test, header=FALSE)
activities_train<-read.table(con_y_train, header=FALSE)
close(con_y_train)
close(con_y_test)


##### read subjects column ###################################
##### I also add a subjects column for clarity ###############

con_sub_test<-file("./UCI HAR Dataset/test/subject_test.txt", "r") 
con_sub_train<-file("./UCI HAR Dataset/train/subject_train.txt", "r") 
sub_test<-read.table(con_sub_test)
sub_train<-read.table(con_sub_train)
close(con_sub_test)
close(con_sub_train)

################################################################
#################################################################
####### QUESTION 1 merge two data frames by row ##############
data_Set<- rbind(file_X_test,file_X_train)

merged_data<-data_Set ### Q1
####### data_Set is the merged set for question  1 ####################

################################################################
#################################################################
######## QUESTION 4  ##### DESCRIPTIVE COL NAMES ###############

###### store original data_set not processed and add column names #####
data_Set_1<-data_Set # make copy of original

features_list$V2<-tolower(features_list$V2)
colnames(data_Set_1) <-features_list$V2

labeled_data_set<-data_Set_1   ## Q4

################################################################
#################################################################
############### QUESTION 2  SELECT MEAN and DEV only ###############

mean_cases<-grep("mean", features_list$V2)
std_cases<-grep("std", features_list$V2)

elements_mean_std<-c(mean_cases, std_cases)
elements_mean_std_sorted<-sort(elements_mean_std)


############### QUESTION 2  SELECT MEAN and DEV only ###############

data_Set_mean_std<-data_Set_1[,elements_mean_std] #### This is the data set subsset for Question 2

################################################################
#################################################################
##### QUESTION 3 name activities with descriptive

subject<-c(sub_test$V1, sub_train$V1)
activity<-c(activities_test$V1, activities_train$V1)

####### ADD columns with activities and subjects   ####################
data_Set_2<-data_Set_mean_std

################################################################
#################################################################
##### QUESTION 3 name activities with descriptive
data_Set_3<-cbind(subject, activity, data_Set_2)
data_Set_4<-data_Set_3


################################################################
#################################################################
###### order by subject and activity ##############################

data_Set_5<-data_Set_4[order(data_Set_4$subject, data_Set_4$activity), ]


##### Get the activity names ##############################

con_activities<-file("./UCI HAR Dataset/activity_labels.txt", "r") 
file_activities<-read.table(con_activities)
close(con_activities)
labels_activities<-as.factor(file_activities$V2)


data_Set_6<-data_Set_5

#subject_factor<-as.factor(as.character(subject))
activities_factor<-as.factor(as.character(activity))

data_Set_6$subject<-as.factor(as.character(data_Set_6$subject))
data_Set_6$activity<-as.factor(as.character(data_Set_6$activity))

levels(data_Set_6$activity) <-labels_activities
data_Set_7<-data_Set_6

labled_activities<-data_Set_7  # Q3


######### QUESTION 5 #############################
##################################################

l_subject<-levels(data_Set_7$subject)
logic_subject<-data_Set_7$subject==l_subject[1]
data_Set_8<-data_Set_7 

#### http://seananderson.ca/2013/10/19/reshape.html

data_Set_9<-melt(data_Set_8, id=c("subject", "activity"))

data_Set_10<-dcast(data_Set_9, 
                   subject + activity ~ variable, value.var="value", mean)

Tidy_Data_Set<-data_Set_10[order(as.numeric(as.character(data_Set_10$subject)), 
                               data_Set_10$activity), ]


########################################################################
##### OUTPUT FILE  Tidy_Data_Set #######################################
########################################################################
########################################################################

Tidy_Data_Set
