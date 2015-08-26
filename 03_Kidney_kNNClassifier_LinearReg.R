## 03_Kidney_kNNClassifier_LinearReg.R
## Written by Cristina Moody
## Aug 2015

# Installing Libraries and functions
# source(file = "00_librariesVNL.R"); # if starting R for 1st time today
# source(file = "01_getKidneyData_v2.R");
# source(file = "02_alignKidneyThreshold.R");

# general libraries
require(lubridate);
# Libraries for regression
require(lmtest);
require(sandwich);
require(car);
require(zoo);
# library for sql
require(sqldf);
# libraries for knn
require(kknn);

# Working directory: "C:/Users/CMoody/Desktop/workspace/VNL"
wDir <- sprintf("%s%s", getwd(), "/");
# Data directory: E:/VNL Data from Joe
dDir <- "E:/KidneyData/";
# SQL database path and name
dbfile <- paste(dDir, "CKD", sep = "");
# Access CKD SQL database
# db <- dbConnect(SQLite(), dbname = dbfile);
# remember to call db <- dbDisconnect(db) when finished!

# Loading all files - it won't work as a function :(
listfiles <- list.files(path = dDir, pattern = "\\.rda");
for (i in 1:length(listfiles)) {
    load(file = sprintf("%s%s", dDir, listfiles[i]));
}

# function to get total number of rows in the data frames in a list
testf <- function(testList){
    # function to get total number of rows in the data frames in a list
    m <- c();
    for (j in 1:length(testList)) {
        m[j] <- sprintf("List %s has %s rows for STUDYID %s",
                        j, nrow(testList[[j]]), testList[[j]]$STUDYID[1]);
    }
    return(m);
}

# Function to create ORDERING_DATE2
addORDDATE2 <- function(ListOfDataFrames) {
    # Function to create ORDERING_DATE2 which is the difference in days from the first lab
    for (j in 1:length(ListOfDataFrames)){
        ListOfDataFrames[[j]]$ORDERING_DATE2 <- ListOfDataFrames[[j]]$ORDERING_DATE -
            ListOfDataFrames[[j]]$ORDERING_DATE[1];
    }
    return(ListOfDataFrames);
}

# Function to start PROPER_TIME for INT_FLAG==0 at some random negative time before 100 days (in hours)
startPTIME <- function(ListOfDataFrames, random = 1){
    # Function to start PROPER_TIME for INT_FLAG==0 at some random negative time before 100 days
    x <- sample(4392:17544, length(ListOfDataFrames), replace = F); # Hours at least 6 months negative
    for (j in 1:length(ListOfDataFrames)){
        if (ListOfDataFrames[[j]]$PROPER_TIME[1] == 0 & ListOfDataFrames[[j]]$INT_FLAG[1] == 0){
            if (!random) {
                y <- as.numeric(ListOfDataFrames[[j]][which.max(ListOfDataFrames[[j]]$LAB_RES_VAL_NUM),]$PROPER_TIME);
                ListOfDataFrames[[j]]$PROPER_TIME <- as.numeric(
                    ListOfDataFrames[[j]]$PROPER_TIME) - y;
            } else if (random) {
                ListOfDataFrames[[j]]$PROPER_TIME <- as.numeric(
                    ListOfDataFrames[[j]]$PROPER_TIME) - x[j];
            }
        }
    }
    return(ListOfDataFrames);
}

# Function to go through list of dataframes and
# replace repeated labs with mean and add standard deviation column
getMeanSDDataFrame <- function(DataFrame) {
    # Function to go through dataframe and replace repeated labs with mean and standard deviation
    # Create new list so not to overwrite original
    newDataFrame <- DataFrame;
    # Function to get the standard deviations of labs repeated on a given day
    sds <- aggregate(LAB_RES_VAL_NUM ~ PROPER_TIME, data = DataFrame, FUN = getSD);
    # Function to get the mean value of labs repeated on a given day
    means <- aggregate(LAB_RES_VAL_NUM ~ PROPER_TIME, data = DataFrame, FUN = mean);
    # Function that merges the hours and keeps the maximum LAB_RES_VAL_NUM - from the internet.
    newDataFrame <- do.call(rbind, lapply(split(newDataFrame, newDataFrame$PROPER_TIME),
                                          function(chunk) chunk[which.max(chunk$LAB_RES_VAL_NUM), ]));
    # Changing the maximum LAB_RES_VAL_NUM to the mean
    newDataFrame$LAB_RES_VAL_NUM <- means$LAB_RES_VAL_NUM;
    # Adding the standard deviation
    newDataFrame$SD_ORD_VAL <- sds$LAB_RES_VAL_NUM;
    return(newDataFrame);
}
# nms creates a character vector with "_day" to rename the columns in a dataframe
nms <- function(n) {
    # nms creates a character vector with "_day" to rename the columns in a dataframe
    ns <- c();
    for (i in -10:0) {
        ns <- cbind( t(paste(n, i, sep = "_")), ns);
    }
    return(t(ns));
}
# Case 1 function
case1 <- function(thisDf, ChangingVars) {
    # Converting a dataframe of patient labs into a single row with only the threshold day and ten days earlier kept.
    # Missing data is filled in with previous good result or last result.
    # Checking that ChangingVars are column names in thisDf
    if (any(ChangingVars %in% names(thisDf)==FALSE)){
        print("COLUMNS LISTED IN ChangingVars NOT PRESENT IN thisDf. BREAK");
        break;
    }
    newDf <- data.frame(PT =(-(10:0)), VT1=NA, VT2=NA, VT3=NA, VT4=NA, VT5=NA, VT7=NA, VT8=NA);
    names(newDf) <- ChangingVars;
    # What this does is reference the row space of newDf that corresponds to a row in thisDf
    # by looking at the value of PT in both of them.
    newDf[ newDf$PROPER_TIME %in% thisDf$PROPER_TIME, ] <- thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ];
    # Change rownames to stringerized version of $PT
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    namesSub <- tail(ChangingVars, -1);
    LastBeforeTen <- as.numeric(thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ]$PROPER_TIME[1]);
    # LastBeforeTen is NA when patient returns.
    if(is.na(LastBeforeTen)){print("NO LastBeforeTen in case1()"); next;}
    newRow <- newDf[ newDf$PROPER_TIME == LastBeforeTen, ];
    newDf <- rbind(newRow, newDf); # Add last measured value before 10 days before threshold
    newDf[namesSub] <- na.locf(newDf[namesSub], fromLast = F)
    newDf[head(ChangingVars, -1)] <- apply(newDf[head(ChangingVars, -1)], 2, function(x) as.numeric(x));
    newDf[order(nrow(newDf):1),] <- newDf; # Invert so that PROPER_TIME counts down from 0 to -10.
    newDf <- head(newDf, -1); # Removing fill row
    finalDf <- as.vector(as.matrix(t(newDf))); # to convert to a row!
    finalnms <- as.vector((nms(ChangingVars))); # Names for new columns
    names(finalDf) <- finalnms; # Change column names
    finalDf <- as.data.frame(t(finalDf)); # Convert to data frame
    # Changing all numbers back to numeric!
    numcols <- as.vector(nms(head(ChangingVars, -1))); # Columns that are numeric
    datcols <- as.vector(nms(tail(ChangingVars, 1))); # Columns that are dates
    finalDf[numcols] <- apply(finalDf[numcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf[datcols] <- apply(finalDf[datcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf <- cbind(finalDf$ORDERING_DATE2_0, finalDf); # Adding Threshold Time to dataframe
    names(finalDf)[names(finalDf)=="finalDf$ORDERING_DATE2_0"] <- "THRESHOLD_TIME";
    return(finalDf);
}
# Case 2 function
case2 <- function(thisDf, ChangingVars) {
    # Converting a dataframe of patient labs into a single row with only the threshold day and ten days earlier kept.
    # Missing data is filled in with previous good result or last result.
    # Checking that ChangingVars are column names in thisDf
    if (any(ChangingVars %in% names(thisDf)==FALSE)){
        print("COLUMNS LISTED IN ChangingVars NOT PRESENT IN thisDf. BREAK");
        break;
    }
    newDf <- data.frame(PT =(-(10:0)), VT1=NA, VT2=NA, VT3=NA, VT4=NA, VT5=NA, VT6=NA, VT7=NA);
    names(newDf) <- ChangingVars;
    # What this does is reference the row space of newDf that corresponds to a row in thisDf
    # by looking at the value of PT in both of them.
    newDf[ newDf$PROPER_TIME %in% thisDf$PROPER_TIME, ] <- thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ];
    # Change rownames to stringerized version of $PT
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    namesSub <- tail(ChangingVars, -1);
    # Getting last filled PROPER_TIME in thisDf in the 10 days before threshold
    LastBeforeTen <- as.numeric(thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ]$PROPER_TIME[1]);
    if (is.na(LastBeforeTen)) {LastBeforeTen <- -10.1;}
    # Getting the measurement before 10 days from threshold
    newRow <- thisDf[ thisDf$PROPER_TIME < LastBeforeTen, ]; # Data frame of values before 10 days before threshold
    newRow <- newRow[nrow(newRow), ]; # first values before 10 days before threshold
    newDf <- rbind(newRow, newDf); # Add last measured value before 10 days before threshold
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    newDf[namesSub] <- na.locf(newDf[namesSub], fromLast = F);
    newDf[head(ChangingVars, -1)] <- apply(newDf[head(ChangingVars, -1)], 2, function(x) as.numeric(x));
    newDf[order(nrow(newDf):1),] <- newDf; # Invert so that PROPER_TIME counts down from 0 to -10.
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    newDf <- head(newDf, -1); # Removing fill row
    finalDf <- as.vector(as.matrix(t(newDf))); # to convert to a row!
    finalnms <- as.vector((nms(ChangingVars))); # Names for new columns
    names(finalDf) <- finalnms; # Change column names
    finalDf <- as.data.frame(t(finalDf)); # Convert to data frame
    # Changing all numbers back to numeric!
    numcols <- as.vector(nms(head(ChangingVars, -1))); # Columns that are numeric
    datcols <- as.vector(nms(tail(ChangingVars, 1))); # Columns that are dates
    finalDf[numcols] <- apply(finalDf[numcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf[datcols] <- apply(finalDf[datcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf <- cbind(newRow$PROPER_TIME, finalDf); # Adding Threshold Time to dataframe
    finalDf$`newRow$PROPER_TIME` <- 0 - finalDf$`newRow$PROPER_TIME`;
    names(finalDf)[names(finalDf)=="newRow$PROPER_TIME"] <- "THRESHOLD_TIME";
    return(finalDf);
}
# Case 3 function
case3 <- function(thisDf, ChangingVars) {
    # Converting a dataframe of patient labs into a single row with only the threshold day and ten days earlier kept.
    # Missing data is filled in with previous good result or last result.
    # Checking that ChangingVars are column names in thisDf
    if (any(ChangingVars %in% names(thisDf)==FALSE)){
        print("COLUMNS LISTED IN ChangingVars NOT PRESENT IN thisDf. BREAK");
        break;
    }
    newDf <- data.frame(PT =(-(10:0)), VT1=NA, VT2=NA, VT3=NA, VT4=NA, VT5=NA, VT7=NA, VT8=NA);
    names(newDf) <- ChangingVars;
    # What this does is reference the row space of newDf that corresponds to a row in thisDf
    # by looking at the value of PT in both of them.
    newDf[ newDf$PROPER_TIME %in% thisDf$PROPER_TIME, ] <- thisDf[ thisDf$PROPER_TIME %in% newDf$PROPER_TIME, ];
    # Change rownames to stringerized version of $PT
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    namesSub <- tail(ChangingVars, -1);
    newDf[namesSub] <- na.locf(newDf[namesSub], fromLast = F);
    newDf[head(ChangingVars, -1)] <- apply(newDf[head(ChangingVars, -1)], 2, function(x) as.numeric(x));
    newDf[order(nrow(newDf):1),] <- newDf; # Invert so that PROPER_TIME counts down from 0 to -10.
    rownames(newDf) = sprintf("%s", newDf$PROPER_TIME); # Rename the row to match PROPER_TIME
    finalDf <- as.vector(as.matrix(t(newDf))); # to convert to a row!
    finalnms <- as.vector((nms(ChangingVars))); # Names for new columns
    names(finalDf) <- finalnms; # Change column names
    finalDf <- as.data.frame(t(finalDf)); # Convert to data frame
    # Changing all numbers back to numeric!
    numcols <- as.vector(nms(head(ChangingVars, -1))); # Columns that are numeric
    datcols <- as.vector(nms(tail(ChangingVars, 1))); # Columns that are dates
    finalDf[numcols] <- apply(finalDf[numcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf[datcols] <- apply(finalDf[datcols], 2, function(x) as.numeric(as.character(x))); # Change columns to numeric
    finalDf <- cbind(finalDf$ORDERING_DATE2_0, finalDf); # Adding Threshold Time to dataframe
    names(finalDf)[names(finalDf)=="finalDf$ORDERING_DATE2_0"] <- "THRESHOLD_TIME";
    return(finalDf);
}
# Creates a training dataframe that will keep track of each point in time
reorderPTKID <- function(ListOfDataFrames, HoursPerTimeStep = 1){
    # Creates a training dataframe that will keep track of each point in time
    # Starts after use of
    #       originalListOfDataFrames <- getResultDateKIDSQL(...);
    #       ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    #       ListOfDataFrames <- getMeanSDListDataFrames(ListOfDataFrames);
    #       ListOfDataFrames <- addORDDATE2(ListOfDataFrames);
    #       ListOfDataFrames <- startPTIME(ListOfDataFrames, 0);
    #       names(ListOfDataFrames[[j]])
    #       [1] "ORDERING_DATE"   "LAB_RES_VAL_NUM" "LAB_RES_UNIT"    "STUDYID"         "orderid"
    #       [6] "encid"           "REFERENCE_HIGH"  "REFERENCE_LOW"   "LAB_COMP_CD"     "LAB_PX_CD"
    #       [11] "MIN_RAW_LABS"   "ORDERING_DATE2"  "PROPER_TIME"     "THRESHOLD_VALUE"
    newOrder <- data.frame();
    varsNotChanging <- c("STUDYID", "MIN_RAW_LABS", "INT_FLAG",
                         "LAB_COMP_CD", "LAB_RES_UNIT",
                         "REFERENCE_LOW", "REFERENCE_HIGH", "THRESHOLD_VALUE");
    varsChanging <- c("PROPER_TIME", "orderid", "encid", "LAB_PX_CD",  #### DO NOT CHANGE THIS VARIABLE (YOU WILL BREAK IT)
                      "LAB_RES_VAL_NUM", "SD_ORD_VAL", "ORDERING_DATE2", "ORDERING_DATE");
    timevars <- c("PROPER_TIME", "ORDERING_DATE2", "ORDERING_DATE");
    for (j in 1:length(ListOfDataFrames)){
        # Changing time to set TimeStep
        ListOfDataFrames[[j]][timevars] <- (ListOfDataFrames[[j]][timevars])/HoursPerTimeStep;
        # flooring time values for integer steps
        ListOfDataFrames[[j]][timevars] <- floor(ListOfDataFrames[[j]][timevars]);
        # Chunking by PROPER_TIME for new TimeStep
        ListOfDataFrames[[j]] <- getMeanSDDataFrame(ListOfDataFrames[[j]]);
        # Changing LAB_PX_CD to character
        ListOfDataFrames[[j]]$LAB_PX_CD <- as.character(ListOfDataFrames[[j]]$LAB_PX_CD);
        # Normalizing LAB_RES_VAL_NUM to LAB_RES_VAL_NUM/REFERENCE_HIGH
        ListOfDataFrames[[j]]$LAB_RES_VAL_NUM <- ListOfDataFrames[[j]]$LAB_RES_VAL_NUM/f2n(ListOfDataFrames[[j]]$REFERENCE_HIGH);
        ListOfDataFrames[[j]]$SD_ORD_VAL <- ListOfDataFrames[[j]]$SD_ORD_VAL/f2n(ListOfDataFrames[[j]]$REFERENCE_HIGH);
        # Creating List to pass to caseKID functions:
        ChangingDF <- ListOfDataFrames[[j]][varsChanging];
        NotChangingDF <- ListOfDataFrames[[j]][varsNotChanging][1,];
        if (as.numeric(min(ChangingDF$PROPER_TIME)) > -10) {
            print(j); print("AAAAAA");
            ChangingDF <- case1(ChangingDF, varsChanging);
        } else if ( as.numeric(min(ChangingDF$PROPER_TIME)) < -10) {
            print(j); print("BBBBBB");
            ChangingDF <- case2(ChangingDF, varsChanging);
        } else if ( as.numeric(min(ChangingDF$PROPER_TIME)) == -10){
            print(j); print("CCCCCC");
            ChangingDF <- case3(ChangingDF, varsChanging);
        } else {print("BREAK"); break;}
        Order <- cbind(NotChangingDF, ChangingDF);
        # Normalizing THRESHOLD_VALUE to THRESHOLD_VALUE/REFERENCE_HIGH
        Order$THRESHOLD_VALUE <- Order$THRESHOLD_VALUE/f2n(Order$REFERENCE_HIGH);
        Order$HoursPerTimeStep <- HoursPerTimeStep;
        rownames(Order) <- sprintf("%s_%s", NotChangingDF$STUDYID, ChangingDF$`ORDERING_DATE_-10`);
        newOrder <- rbind(Order, newOrder);
    }
    return(newOrder);
}

# Getting matrix for 'meta' patient for regression from lists
getTimeTrainMatrix <- function(originalListOfDataFrames, random = 1, HoursPerTimeStep = 1){
    # Getting matrix for 'meta' patient for regression from lists in time
    # Function returnProperTime() from alignThreshold.R - returns PROPER_TIME and INT_FLAG
    ListOfDataFrames <- returnProperTime(originalListOfDataFrames); # def in 02_alignKidneyThreshold.R
    if (class(ListOfDataFrames)=='numeric') {print("NO SERIES PASSED PROPER_TIME CUT"); break;}
    # Removing labs that are repeated on the same day and replacing with mean and sd
    ListOfDataFrames <- getMeanSDListDataFrames(ListOfDataFrames); # def in 02_alignKidneyThreshold.R
    # Creating ORDERING_DATE2
    ListOfDataFrames <- addORDDATE2(ListOfDataFrames); # def above
    # Starting patients that do not cross threshold at random
    # negative days between 6 months and 2 years before threshold (in hours)
    ListOfDataFrames <- startPTIME(ListOfDataFrames, random); # def above
    # Organizing into giant dataframe with only 10 days before threshold (in hours)
    TrainDF <- reorderPTKID(ListOfDataFrames, HoursPerTimeStep); # def above
    #     # Subset TrainDF into only interesting cases (INT_FLAG==1) # Oleg said to remove
    #     TrainDF <- TrainDF[TrainDF$INT_FLAG==1, ];
    # return final dataframe
    return(TrainDF);
}


# Not ordered in time:
# Getting matrix for 'meta' patient for regression
# from lists use getTrainMatrix() on all Lists of Dataframes
getTrainMatrix <- function(originalListOfDataFrames){
    # Getting matrix for 'meta' patient for regression from lists
    # Function returnProperTime() from alignThreshold.R - returns PROPER_TIME and INT_FLAG
    ListOfDataFrames <- returnProperTime(originalListOfDataFrames);
    # Removing labs that are repeated on the same day and replacing with mean and sd
    ListOfDataFrames <- getMeanSDListDataFrames(ListOfDataFrames);
    # Creating ORDERING_DATE2
    ListOfDataFrames <- addORDDATE2(ListOfDataFrames);
    # Starting patients that do not cross threshold at random
    # negative days between 6 months and 2 years before threshold
    ListOfDataFrames <- startPTIME(ListOfDataFrames);
    # Creating giant dataframe of all the dataframes
    TrainDF <- getTrainDF(ListOfDataFrames);
    #     # Subset TrainDF into only interesting cases (INT_FLAG==1) # Oleg said to remove
    #     TrainDF <- TrainDF[TrainDF$INT_FLAG==1, ];
    # Linear regression package cannot use days, so changing to numeric
    TrainDF$PROPER_TIME <- as.numeric(TrainDF$PROPER_TIME);
    TrainDF$ORDERING_DATE2 <- as.numeric(TrainDF$ORDERING_DATE2);
    # Normalizing LAB_RES_VAL_NUM to LAB_RES_VAL_NUM/REFERENCE_HIGH
    TrainDF$LAB_RES_VAL_NUM <- TrainDF$LAB_RES_VAL_NUM/f2n(TrainDF$REFERENCE_HIGH);
    TrainDF$SD_ORD_VAL <- TrainDF$SD_ORD_VAL/f2n(TrainDF$REFERENCE_HIGH);
    # Normalizing THRESHOLD_VALUE to THRESHOLD_VALUE/REFERENCE_HIGH
    TrainDF$THRESHOLD_VALUE <- TrainDF$THRESHOLD_VALUE/f2n(TrainDF$REFERENCE_HIGH);
    # Adding row names for regression
#     rownames(TrainDF) <- paste(TrainDF$ORD_NUM_VALUE, "+/-", TrainDF$SD_ORD_VAL,
#                                TrainDF$REFERENCE_UNIT, TrainDF$CPT_CODE,
#                                TrainDF$COMPONENT_ID, TrainDF$ORDERING_DATE2,
#                                TrainDF$PROPER_TIME, TrainDF$ORDERING_DATE,
#                                TrainDF$PAT_ID, sep = "_");
    # return final dataframe
    return(TrainDF);
}
# Creating giant dataframe of all the dataframes
getTrainDF <- function(ListOfDataFrames){
    # Creating giant dataframe of all the dataframes
    TrainDF <- data.frame();
    for (j in 1:length(ListOfDataFrames)) {
        TrainDF <- rbind(ListOfDataFrames[[j]], TrainDF);
    }
    return(TrainDF);
}
