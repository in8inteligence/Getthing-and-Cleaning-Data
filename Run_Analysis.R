---
title: "Week 4 Assignment"
author: "Alan Jenks"
date: "14/05/2019"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Week 4 Assignement Getting and Cleaning Data

You should create one R script called run_analysis.R that does the following.

1.Merges the training and the test sets to create one data set.
2.Extracts only the measurements on the mean and standard deviation for each measurement.
3.Uses descriptive activity names to name the activities in the data set
4.Appropriately labels the data set with descriptive variable names.
5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

###### 1. Merge the training and the test sets to create one data set.

```{r}
setwd('C:/Users/hp/Downloads/getdata_projectfiles_UCI HAR Dataset/UCI HAR Dataset');
```


# Import training data from files & Name the columns 

```{r}
features <- read.table('./features.txt',header=FALSE);
activityLabels <- read.table('./activity_labels.txt',header=FALSE); 
        colnames(activityLabels) <- c("activityId","activityType");
subjectTrain <- read.table('./train/subject_train.txt',header=FALSE); 
        colnames(subjectTrain) <- "subjectId";
xTrain <- read.table('./train/x_train.txt',header=FALSE); colnames(xTrain) <- 
                features[,2];
yTrain <- read.table('./train/y_train.txt',header=FALSE); colnames(yTrain) <- 
                "activityId";
```

# Merge Data into complete training set

```{r}
trainingSet = cbind(yTrain,subjectTrain,xTrain);
```

# Import test data from files & Name columns

```{r}
subjectTest <- read.table('./test/subject_test.txt',header=FALSE); 
        colnames(subjectTest) <- "subjectId";
xTest <- read.table('./test/x_test.txt',header=FALSE); colnames(xTest) <- 
        features[,2];
yTest <- read.table('./test/y_test.txt',header=FALSE); colnames(yTest) <- 
        "activityId";
```


# Merge Data into complete test set

```{r}
testSet = cbind(yTest,subjectTest,xTest);
```


# Combine Training Data Set and Test Data Set into one Merged Data Set

```{r}
MergedDataSet = rbind(trainingSet,testSet);
```

# Create columns vector to prepare data for subsetting

```{r}
columns <- colnames(MergedDataSet);
```

###### 2. Extract only the measurements on the mean and standard deviation for each measurement

# Create a vector that indentifies the ID, mean & stddev columns as TRUE

```{r}
vector <- (grepl("activity..",columns) | grepl("subject..",columns) | grepl("-mean..",columns) &
                  !grepl("-meanFreq..",columns) & !grepl("mean..-",columns) | 
                        grepl("-std..",columns) & !grepl("-std()..-",columns));
```


# Update MergedDataSet based on previously identified columns
```{r}
MergedDataSet <- MergedDataSet[vector==TRUE];
```

###### 3. Use descriptive activity names to name the activities in the data set

# Add in descriptive activity names to MergedDataSet & update columns vector

```{r}
MergedDataSet <- merge(MergedDataSet,activityLabels,by='activityId',all.x=TRUE);
        MergedDataSet$activityId <-activityLabels[,2][match(MergedDataSet$activityId, activityLabels[,1])] 

columns <- colnames(MergedDataSet);
```


###### 4. Appropriately label the data set with descriptive activity names.

# Tidy column names

```{r}
for (i in 1:length(columns)) 
        {
                columns[i] <- gsub("\\()","",columns[i])
                columns[i] <- gsub("-std$","StdDev",columns[i])
                columns[i] <- gsub("-mean","Mean",columns[i])
                columns[i] <- gsub("^(t)","time",columns[i])
                columns[i] <- gsub("^(f)","freq",columns[i])
                columns[i] <- gsub("([Gg]ravity)","Gravity",columns[i])
                columns[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",columns[i])
                columns[i] <- gsub("[Gg]yro","Gyro",columns[i])
                columns[i] <- gsub("AccMag","AccMagnitude",columns[i])
                columns[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",columns[i])
                columns[i] <- gsub("JerkMag","JerkMagnitude",columns[i])
                columns[i] <- gsub("GyroMag","GyroMagnitude",columns[i])
        };
```

        
# Update MergedDataSet with new descriptive column names

```{r}
colnames(MergedDataSet) <- columns;
```


# Remove activityType column

```{r}
MergedDataSet <- MergedDataSet[,names(MergedDataSet) != 'activityType'];
```


###### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Averaging each activity and each subject as Tidy Data

```{r}
tidyData <- aggregate(MergedDataSet[,names(MergedDataSet) 
                != c('activityId','subjectId')],by=list
                        (activityId=MergedDataSet$activityId,
                                subjectId=MergedDataSet$subjectId),mean);
```


# Export tidyData set 
```{r}
write.table(tidyData, './FinalTidyData.txt',row.names=FALSE,sep='\t')
```


