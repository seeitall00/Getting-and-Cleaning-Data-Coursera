--------------------------------------------------------------------------
---------------------getting the raw data --------------------------------
# reading in the Data Sets

subject_test <- read.table(file.path(getwd(), "UCI HAR Dataset/test/subject_test.txt"))

x_test <- read.table(file.path(getwd(),"UCI HAR Dataset/test/x_test.txt"))

y_test <- read.table(file.path(getwd(),"UCI HAR Dataset/test/y_test.txt"))

subject_train <- read.table(file.path(getwd(),"UCI HAR Dataset/train/subject_train.txt"))

x_train <- read.table(file.path(getwd(),"UCI HAR Dataset/train/X_train.txt"))

y_train <- read.table(file.path(getwd(),"UCI HAR Dataset/train/y_train.txt"))

features <- read.table(file.path(getwd(),"UCI HAR Dataset/features.txt"))

activities <- read.table(file.path(getwd(),"UCI HAR Dataset/activity_labels.txt"))

--------------------------------------------------------------------------
---------------------creating the train dataset---------------------------
# rename Variable V1 in Subject_train
subject_train2 <- rename(subject_train, subjectID = V1)
# rename all Variables in x_train
x_train_features <- setNames(x_train, as.vector(features[,2]))
# join subject and x_train
x_train_features_subject <- cbind(subject_train2,x_train_features)
# rename Variable in y_train
y_train2 <- rename(y_train, Labels = V1)
# join y_train onto other Data
x_train_features_subject_y <- cbind(x_train_features_subject,y_train2)
--------------------------------------------------------------------------
---------------------creating the test dataset----------------------------
# rename Variable in Subject_test
subject_test2 <- rename(subject_test, subjectID = V1)
# rename all Variables in x_test
x_test_features <- setNames(x_test, as.vector(features[,2]))
# join subject and x_test
x_test_features_subject <- cbind(subject_test2,x_test_features)
# rename Variable in y_test
y_test2 <- rename(y_test, Labels = V1)
# join y_test onto other Data
x_test_features_subject_y <- cbind(x_test_features_subject,y_test2)
--------------------------------------------------------------------------
---------------merging the Datasets and making them tidy------------------
# merging test and train
test_train <- rbind(x_test_features_subject_y, x_train_features_subject_y)
# finding all mean Values. No spezification onto what form of mean so meanFreq is included
test_train_select <-  test_train[c('subjectID','Labels',names(test_train[, grep('mean', names(test_train), fixed = TRUE)]),names(test_train[, grep('std', names(test_train), fixed = TRUE)]))]
# swapping the lables for the activities
# first rename Variable to match with the ID in the Table i want to join with
activities2 <- rename(activities, 'Labels' = V1) 
# joining the actual Activity Name to the Tabel
test_train_select_act <- merge(activities2,test_train_select)
# renaming the Column to a real Name
test_train_select_act2 <- rename(test_train_select_act, 'Activities' = V2)
# removing the ID Label. it has no use anymore since i have the actual Activity name now
fullset <- select(test_train_select_act2,-(Labels))
--------------------------------------------------------------------------
-------------------doing the maths----------------------------------------
# calculating the averages per activity and subject
grouped_table <- fullset%>% group_by(Activities, subjectID) %>% summarise_each(funs(mean))

write.table(grouped_table,"tidy_data.txt", row.name=FALSE)
                                       