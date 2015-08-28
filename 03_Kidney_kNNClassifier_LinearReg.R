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
startPTIME <- function(ListOfDataFrames, random = 0){
    # Function to start PROPER_TIME for INT_FLAG==0 at
    # random = 1: some random negative time before 100 days
    # random = 0: aligns maximum lab value with threshold value
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
            # print(j); print("AAAAAA");
            ChangingDF <- case1(ChangingDF, varsChanging);
        } else if ( as.numeric(min(ChangingDF$PROPER_TIME)) < -10) {
            # print(j); print("BBBBBB");
            ChangingDF <- case2(ChangingDF, varsChanging);
        } else if ( as.numeric(min(ChangingDF$PROPER_TIME)) == -10){
            # print(j); print("CCCCCC");
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
#     rownames(TrainDF) <- paste(TrainDF$LAB_RES_VAL_NUM, "+/-", TrainDF$SD_ORD_VAL,
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

makeTimeTrain <- 0; HoursPerTimeStep <- 4; random <- 0; ## Set parameters HERE!
if (makeTimeTrain){
    TimeTrain_K <- getTimeTrainMatrix(K_1520_gt20, random, HoursPerTimeStep);
    TimeTrain_P <- getTimeTrainMatrix(P_1555_gt20, random, HoursPerTimeStep);
    TimeTrain_Creat <- getTimeTrainMatrix(CREAT_1523_gt20, random, HoursPerTimeStep);

    # Making into one list
    goodKidneyDataOrdered10StepsBeforeThreshold <- rbind(TimeTrain_K, TimeTrain_Creat);
    goodKidneyDataOrdered10StepsBeforeThreshold <- rbind(goodKidneyDataOrdered10StepsBeforeThreshold, TimeTrain_P);
    # Renaming and saving data
    goodKidneyDataOrdered10StepsBeforeThreshold_alignMax <- goodKidneyDataOrdered10StepsBeforeThreshold;
    save(goodKidneyDataOrdered10StepsBeforeThreshold_alignMax,
         TimeTrain_K, TimeTrain_P, TimeTrain_Creat,
         file = sprintf("%s%s", dDir, "TimeOrderedKidney_data_gt20_alignedMax.rda"));
} else {load(file = sprintf("%s%s", dDir, "TimeOrderedKidney_data_gt20_alignedMax.rda"));)}

makePop <- 0;
getPopData <- function(dbfile){
    # Getting population data CHOSE VARIABLES FOR CLASSIFIER HERE!
    # Query for fields in the lab_orders_m.csv and
    # esrd_patients_m.csv (popQuery1) or ckd_patients_m.csv (popQuery2) files
    popQuery1="
    SELECT
    t1.STUDYID,
    t2.M_AGE,
    t2.M_BIRTH_DATE,
    t2.M_DEATH_DATE,
    t2.GENDER,
    t2.RACE
    FROM
    lab_orders_m as t1
    INNER JOIN esrd_patients_m as t2
    ON t1.STUDYID = t2.STUDYID";
    popQuery2="
    SELECT
    t1.STUDYID,
    t2.M_AGE,
    t2.M_BIRTH_DATE,
    t2.M_DEATH_DATE,
    t2.GENDER,
    t2.RACE
    FROM
    lab_orders_m as t1
    INNER JOIN patients_m as t2
    ON t1.STUDYID = t2.STUDYID";
    popdata1 <- getDF(dbfile, popQuery1); # def in 01_getKidneyData_v2.R
    popdata2 <- getDF(dbfile, popQuery2); # def in 01_getKidneyData_v2.R
    popdata <- rbind(popdata1, popdata2);
    popdata <- unique.data.frame(popdata); ### NOTE THAT SOME STUDYID HAVE MORE THAN ONE AGE! NOT SURE HOW TO FIX
}
if (makePop) {
    popdata <- getPopData(dbfile);
    save(popdata, file = sprintf("%s%s", dDir, "popdata.rda"));
} else {load(file = sprintf("%s%s", dDir, "popdata.rda"));}

# Merging population and lab data
mergedDF <-  merge.data.frame(goodKidneyDataOrdered10StepsBeforeThreshold_alignMax,
                              popdata, by = "STUDYID");
mergedDF <- unique.data.frame(mergedDF);
length(unique(mergedDF$STUDYID));
# [1] 1065
length(unique(goodKidneyDataOrdered10StepsBeforeThreshold_alignMax$STUDYID));
# [1] 1065
# same total number of patients, not sure how to deal with multiple ages in popdata

# Function to return set the threshold value in a dataframe for the following labs
setThreshold <- function(DataFrame){
    # Function to return set the threshold value in a dataframe for the following labs
    tID_CREAT = 1523; THRESHOLD_CREAT = 3;
    tID_K = 1520; THRESHOLD_K = 6;
    tID_P = 1555; THRESHOLD_P = 4; # Something is wrong here, the threshold is below the REFERENCE_HIGH
    if (is.null(nrow(DataFrame))){
        print(i);
        return("ERROR IN t0Finder, DataFrame has no rows!");
    }
    for (i in 1:nrow(DataFrame)){
        if (DataFrame$LAB_COMP_CD[i] == tID_CREAT) {DataFrame$THRESHOLD_VALUE[i] <- THRESHOLD_CREAT;}
        else if (DataFrame$LAB_COMP_CD[i] == tID_K) {DataFrame$THRESHOLD_VALUE[i] <- THRESHOLD_K;}
        else if (DataFrame$LAB_COMP_CD[i] == tID_P) {DataFrame$THRESHOLD_VALUE[i] <- THRESHOLD_P;}
        else {return("ERROR IN t0Finder, Lab does not have a THRESHOLD_VALUE!")}
    }
    return(DataFrame);
}
createMeta <- function(mDF){
    # Function takes merged dataframe of the time population data and Ordered10DaysBeforeThreshold
    # to create and clean up a new dataframe that is returned.
    mergedDFg <- mDF;
    tID_CREAT = 1523; THRESHOLD_CREAT = 3;
    tID_K = 1520; THRESHOLD_K = 6;
    tID_P = 1555; THRESHOLD_P = 4; # Something is wrong here, the threshold is below the REFERENCE_HIGH

    # Fixing REFERENCE_LOW
    #     summary(table(mergedDFg$REFERENCE_LOW));
    #     summary((mergedDFg$REFERENCE_LOW));
    #     class(mergedDFg$REFERENCE_LOW);
    faux <- mergedDFg[is.na(mergedDFg$REFERENCE_LOW),];
    faux <- setThreshold(faux);
    faux[faux$LAB_COMP_CD==tID_K
         & is.na(faux$REFERENCE_LOW), ]$REFERENCE_LOW <- mergedDFg[!is.na(mergedDFg$REFERENCE_LOW)
                                                                   & mergedDFg$LAB_COMP_CD==tID_K,]$REFERENCE_LOW[1];
    faux[faux$LAB_COMP_CD==tID_K
         & is.na(faux$REFERENCE_HIGH), ]$REFERENCE_HIGH <- mergedDFg[!is.na(mergedDFg$REFERENCE_HIGH)
                                                                     & mergedDFg$LAB_COMP_CD==tID_K,]$REFERENCE_HIGH[1];
    faux[faux$LAB_COMP_CD==tID_CREAT
         & is.na(faux$REFERENCE_LOW), ]$REFERENCE_LOW <- mergedDFg[!is.na(mergedDFg$REFERENCE_LOW)
                                                                   & mergedDFg$LAB_COMP_CD==tID_CREAT,]$REFERENCE_LOW[1];
    faux[faux$LAB_COMP_CD==tID_CREAT
         & is.na(faux$REFERENCE_HIGH), ]$REFERENCE_HIGH <- mergedDFg[!is.na(mergedDFg$REFERENCE_HIGH)
                                                                     & mergedDFg$LAB_COMP_CD==tID_CREAT,]$REFERENCE_HIGH[1];
    mergedDFg[is.na(mergedDFg$REFERENCE_LOW),] <- faux;
    mergedDFg$REFERENCE_LOW <- f2n(mergedDFg$REFERENCE_LOW)
    # Fixing REFERENCE_HIGH
    #     summary(table(mergedDFg$REFERENCE_HIGH));
    #     summary((mergedDFg$REFERENCE_HIGH));
    #     class(mergedDFg$REFERENCE_HIGH);
    mergedDFg$REFERENCE_HIGH <- f2n(mergedDF$REFERENCE_HIGH);
    faux2 <-  mergedDFg[is.na(mergedDFg$REFERENCE_HIGH), ];

    faux2[faux2$LAB_COMP_CD==tID_CREAT
         & is.na(faux2$REFERENCE_HIGH), ]$REFERENCE_HIGH <- mergedDFg[!is.na(mergedDFg$REFERENCE_HIGH)
                                                                     & mergedDFg$LAB_COMP_CD==tID_CREAT,]$REFERENCE_HIGH[1];
    faux2[faux2$LAB_COMP_CD==tID_K
         & is.na(faux2$REFERENCE_HIGH), ]$REFERENCE_HIGH <- mergedDFg[!is.na(mergedDFg$REFERENCE_HIGH)
                                                                     & mergedDFg$LAB_COMP_CD==tID_K,]$REFERENCE_HIGH[1];
    mergedDFg[is.na(mergedDFg$REFERENCE_HIGH), ] <- faux2;
    # Fixing M_AGE
    #     str(mergedDFg$M_AGE);
    #     summary(table(mergedDFg$M_AGE));
    #     summary((mergedDFg$M_AGE));
    #     class(mergedDFg$M_AGE);
    mergedDFg$M_AGE <- as.factor(mergedDF$M_AGE); ### THIS APPEARS INCORRECT
    # Fixing GENDER
    mergedDFg$GENDER <- as.factor(mergedDF$GENDER);
    # Fixing RACE
    #     str(mergedDFg$RACE);
    #     summary(table(mergedDFg$RACE));
    #     summary((mergedDFg$RACE));
    #     class(mergedDFg$RACE);
    mergedDFg$RACE <- as.factor(mergedDF$RACE);
    # Fixing M_DEATH_DATE
    # Setting NA to 99999999
    #     str(mergedDFg$M_DEATH_DATE);
    #     summary((mergedDFg$M_DEATH_DATE));
    #     summary(table(mergedDFg$M_DEATH_DATE));
    #     class(mergedDFg$M_DEATH_DATE);
    mergedDFg[is.na(mergedDFg$M_DEATH_DATE),]$M_DEATH_DATE <- 99999999;
    # Removing remaining NA in AGE, could be fixed by looking at M_BIRTH_DATE and ORDERING_DATE_0
    df <- mergedDFg;
    df[apply(is.na(df), 1, any), ]; # shows 8 rows with <NA> in M_AGE
    # Removing these rows
    mergedDFg <- mergedDFg[!is.na(mergedDFg$M_AGE), ];

    # Columns to remove from classification
    # removCol <- c("MIN_RAW_LABS", "HoursPerTimeStep");

    # str(mergedDFg, list.len = 999);
    meta <- subset(mergedDFg, select=-c(MIN_RAW_LABS, HoursPerTimeStep));
    return(meta);
}

meta <- createMeta(mergedDF);

# Introducing NA's so abandoning below, may be useful later
# # Separating AGE into decade bins for classifier
# meta$AGE2[f2n(meta$M_AGE) < 10 & f2n(meta$M_AGE) >=  0] <- '0-10';
# meta$AGE2[f2n(meta$M_AGE) < 20 & f2n(meta$M_AGE) >= 10] <- '10-20';
# meta$AGE2[f2n(meta$M_AGE) < 30 & f2n(meta$M_AGE) >= 20] <- '20-30';
# meta$AGE2[f2n(meta$M_AGE) < 40 & f2n(meta$M_AGE) >= 30] <- '30-40';
# meta$AGE2[f2n(meta$M_AGE) < 50 & f2n(meta$M_AGE) >= 40] <- '40-50';
# meta$AGE2[f2n(meta$M_AGE) < 60 & f2n(meta$M_AGE) >= 50] <- '50-60';
# meta$AGE2[f2n(meta$M_AGE) < 70 & f2n(meta$M_AGE) >= 60] <- '60-70';
# meta$AGE2[f2n(meta$M_AGE) < 80 & f2n(meta$M_AGE) >= 70] <- '70-80';
# meta$AGE2[f2n(meta$M_AGE) <= 150 & f2n(meta$M_AGE) >= 80] <- '80+';
# meta$AGE2[meta$AGE==">85" ] <- '80+';
# meta$AGE2 <- factor(meta$AGE2, labels = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80+"))

# Checking for NA's: NONE!
naCol <- as.data.frame(apply(meta,2,function(x) any(is.na(x)) ));

# Creating training and test set for classification and regression.
set.seed(42);
n.points <- nrow(meta);
sampling.rate <- 0.8;
num.test.set.labels <- n.points * (1 - sampling.rate);
training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE);
kid.train <- subset(meta[training, ]);
testing <- setdiff(1:n.points, training);
kid.test <- subset(meta[testing, ]);

## CLASSIFYING FOR INT_FLAG (Need to put this in a function if possible)
diagnostic <- 0;
if (diagnostic) {
# Trying binary classifier to predict if patient belongs to INT_FLAG==1 category.
flag.cut <- 0.5; # Because classifier returns a continous value between 0-1, not 0 or 1
kid.knn.flag <- train.kknn(formula = formula(`INT_FLAG` ~ .),
                           data = kid.train, kmax = 50, distance = 1);
kid.knn.flag;
# Call:
# train.kknn(formula = formula(INT_FLAG ~ .), data = kid.train,     kmax = 50, distance = 1)
#
# Type of response variable: continuous
# minimal mean absolute error: 0.1347695
# Minimal mean squared error: 0.1287463
# Best kernel: optimal
# Best k: 2
kid.knn.flag1 <- kid.knn.flag$fitted.values[[kid.knn.flag$best.parameters$k]][1: nrow(kid.train)];
# Need 1 or 0
kid.knn.flag1 <-  vapply(kid.knn.flag1, FUN = function(x){if (x > flag.cut) 1 else 0}, FUN.VALUE = c(1));
CM.flag1 <- table(kid.knn.flag1, kid.train$INT_FLAG);
CM.flag1;
# kid.knn.flag1    0    1
#               0 1209  141
#               1  134  512
# table(kid.train$INT_FLAG,kid.train$INT_FLAG)
#                   0    1
#              0 1352    0
#              1    0  644
accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
accuracy.flag1; # [1] 0.8622244
prediction.flag1 <- predict(kid.knn.flag, kid.test);
prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > flag.cut) 1 else 0},
                            FUN.VALUE = c(1));
# Confusion Matrix
CM.prediction.flag1 <- table(prediction.flag1, kid.test$INT_FLAG);
CM.prediction.flag1;
# prediction.flag1      0  1
#                   0 307  42
#                   1  41 110
# table(kid.test$INT_FLAG, kid.test$INT_FLAG);
#                       0   1
#                   0 339   0
#                   1   0 161
accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
accuracy.prediction.flag1; # [1] 0.834

# Finding best flag.cut
findbest.flag.cut <- data.frame(flag.cut = numeric(0),
                                test.accuracy = numeric(0), predict.accuracy = numeric(0));
for (j in seq(0, 1, 0.05)){
    # print(j); flag.cut <- j;
    kid.knn.flag1 <- kid.knn.flag$fitted.values[[3]][1: nrow(kid.train)];
    kid.knn.flag1 <-  vapply(kid.knn.flag1, FUN = function(x){if (x > j) 1 else 0}, FUN.VALUE = c(1));
    CM.flag1 <- table(kid.knn.flag1, kid.train$INT_FLAG);
    accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
    # print(accuracy.flag1);
    prediction.flag1 <- predict(kid.knn.flag, kid.test);
    prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > j) 1 else 0},
                                FUN.VALUE = c(1));
    CM.prediction.flag1 <- table(prediction.flag1, kid.test$INT_FLAG);
    accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
    # print(accuracy.prediction.flag1);
    findbest.flag.cut <- rbind(findbest.flag.cut, data.frame(flag.cut = j,
                                                             test.accuracy = accuracy.flag1,
                                                             predict.accuracy = accuracy.prediction.flag1))
}
plot(findbest.flag.cut$flag.cut, findbest.flag.cut$test.accuracy, panel.first = grid())
plot(findbest.flag.cut$flag.cut, findbest.flag.cut$predict.accuracy, panel.first = grid())
findbest.flag.cut[which.max(findbest.flag.cut$test.accuracy),];
#   flag.cut test.accuracy predict.accuracy
# 8     0.35     0.8622244            0.834
# 14    0.35     0.8622244            0.834
findbest.flag.cut[which.max(findbest.flag.cut$predict.accuracy),];
#   flag.cut test.accuracy predict.accuracy
# 8     0.35     0.8622244            0.834
# 14    0.35     0.8622244            0.834
{#
# # Again, finer
# findbest.flag.cut <- data.frame(flag.cut = numeric(0),
#                                 test.accuracy = numeric(0), predict.accuracy = numeric(0));
# for (j in seq(0.40, 0.60, 0.005)){ # finding best value for flag.cut between 0.45 and 0.65
#     # print(j); flag.cut <- j;
#     kid.knn.flag1 <- kid.knn.flag$fitted.values[[3]][1: nrow(kid.train)];
#     kid.knn.flag1 <-  vapply(kid.knn.flag1, FUN = function(x){if (x > j) 1 else 0}, FUN.VALUE = c(1));
#     CM.flag1 <- table(kid.knn.flag1, kid.train$INT_FLAG);
#     accuracy.flag1 <- (sum(diag(CM.flag1)))/sum(CM.flag1);
#     # print(accuracy.flag1);
#     prediction.flag1 <- predict(kid.knn.flag, kid.test);
#     prediction.flag1 <-  vapply(prediction.flag1, FUN = function(x){if (x > j) 1 else 0},
#                                 FUN.VALUE = c(1));
#     CM.prediction.flag1 <- table(prediction.flag1, kid.test$INT_FLAG);
#     accuracy.prediction.flag1 <- (sum(diag(CM.prediction.flag1)))/sum(CM.prediction.flag1);
#     # print(accuracy.prediction.flag1);
#     findbest.flag.cut <- rbind(findbest.flag.cut, data.frame(flag.cut = j,
#                                                              test.accuracy = accuracy.flag1,
#                                                              predict.accuracy = accuracy.prediction.flag1))
# }
# plot(findbest.flag.cut$flag.cut, findbest.flag.cut$test.accuracy, panel.first = grid())
# plot(findbest.flag.cut$flag.cut, findbest.flag.cut$predict.accuracy, panel.first = grid())
# findbest.flag.cut[which.max(findbest.flag.cut$test.accuracy),];
#   flag.cut test.accuracy predict.accuracy
#      0.4     0.8622244            0.834
#      0.6     0.8622244            0.834
# Setting flag.cut to best f value of 0.5 on line 535
}
}
# Need to make classifier into a function. Pass it the full data set, desired split
# for training and testing. Have it return the full data set with predicted INT_FLAG
#  and formula from classification
INTkNNclassifier <- function(metaBDF, percentTrain, flag.cut = 0.5){
    # Function to classify each STUDYID predicted INT_FLAG.
    # Returns a list with formula, and data with PrINT_FLAG

    # Creating training and test set for classification and regression.
    set.seed(42);
    n.points <- nrow(metaBDF);
    sampling.rate <- percentTrain;
    num.test.set.labels <- n.points * (1 - sampling.rate);
    training <- sample(1:n.points, sampling.rate * n.points, replace = FALSE);
    kid.train <- subset(metaBDF[training, ]);
    testing <- setdiff(1:n.points, training);
    kid.test <- subset(metaBDF[testing, ]);

    # Nearest Neightboors Classifier
    kid.knn.flag <- train.kknn(formula = formula(`INT_FLAG` ~ .),
                               data = kid.train, kmax = 50, distance = 1);
    full.predict <- <- predict(kid.knn.flag, metaBDF);
    full.predict <-  vapply(full.predict, FUN = function(x){if (x > flag.cut) 1 else 0},
                                 FUN.VALUE = c(1));

    # Full data frame with new column
    metaBDG$PrINT_FLAG <- full.predict.flag;
    # Removing Measured INT_FLAG
    mINT_FLAG <- metaBDF$INT_FLAG;
}
# full PrINT_FLAG for the multiple generalized linear regression
full.predict.flag <- predict(kid.knn.flag, meta);
full.predict.flag <-  vapply(full.predict.flag, FUN = function(x){if (x > flag.cut) 1 else 0},
                             FUN.VALUE = c(1));
# Full data frame with new column
meta$PrINT_FLAG <- full.predict.flag;
# Removing Measured INT_FLAG
mINT_FLAG <- meta$INT_FLAG;
# Data sets for multiple linear regression
metaR <- subset(meta, select=-c(INT_FLAG));
kid.train.R <- subset(metaR[training, ]);
kid.test.R <- subset(metaR[testing, ]);
## Linear Regression
{## Linear regression
reg3 <- lm(`THRESHOLD_TIME` ~ `LAB_COMP_CD` + `LAB_PX_CD_0` + PrINT_FLAG +
               `LAB_RES_VAL_NUM_0` + `LAB_RES_VAL_NUM_-1` + `LAB_RES_VAL_NUM_-2` +
               `LAB_RES_VAL_NUM_-3` + `LAB_RES_VAL_NUM_-4` + `LAB_RES_VAL_NUM_-5` +
               `LAB_RES_VAL_NUM_-6` + `LAB_RES_VAL_NUM_-7` + `LAB_RES_VAL_NUM_-8` +
               `LAB_RES_VAL_NUM_-9` + `LAB_RES_VAL_NUM_-10` +
               `ORDERING_DATE2_0` + `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` +
               `ORDERING_DATE2_-3` + `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` +
               `ORDERING_DATE2_-6` + `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` +
               `ORDERING_DATE2_-9` + `ORDERING_DATE2_-10`,
           data = kid.train.R);
summary(reg3);
# Call:
#     lm(formula = THRESHOLD_TIME ~ LAB_COMP_CD + LAB_PX_CD_0 + PrINT_FLAG +
#            LAB_RES_VAL_NUM_0 + `LAB_RES_VAL_NUM_-1` + `LAB_RES_VAL_NUM_-2` +
#            `LAB_RES_VAL_NUM_-3` + `LAB_RES_VAL_NUM_-4` + `LAB_RES_VAL_NUM_-5` +
#            `LAB_RES_VAL_NUM_-6` + `LAB_RES_VAL_NUM_-7` + `LAB_RES_VAL_NUM_-8` +
#            `LAB_RES_VAL_NUM_-9` + `LAB_RES_VAL_NUM_-10` + ORDERING_DATE2_0 +
#            `ORDERING_DATE2_-1` + `ORDERING_DATE2_-2` + `ORDERING_DATE2_-3` +
#            `ORDERING_DATE2_-4` + `ORDERING_DATE2_-5` + `ORDERING_DATE2_-6` +
#            `ORDERING_DATE2_-7` + `ORDERING_DATE2_-8` + `ORDERING_DATE2_-9` +
#            `ORDERING_DATE2_-10`, data = kid.train.R)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -26.27   -5.93   -4.52   -2.13 1631.54
#
# Coefficients:
#     Estimate Std. Error t value Pr(>|t|)
# (Intercept)            3.600e+02  4.870e+02   0.739    0.460
# LAB_COMP_CD           -2.764e-01  3.301e-01  -0.837    0.403
# LAB_PX_CD_0            8.618e-04  1.095e-03   0.787    0.432
# PrINT_FLAG             5.355e-01  3.338e+00   0.160    0.873
# LAB_RES_VAL_NUM_0     -6.938e-01  2.358e+00  -0.294    0.769
# `LAB_RES_VAL_NUM_-1`  -1.273e+01  4.904e+01  -0.260    0.795
# `LAB_RES_VAL_NUM_-2`   2.275e+01  5.496e+01   0.414    0.679
# `LAB_RES_VAL_NUM_-3`   1.055e+01  3.519e+01   0.300    0.764
# `LAB_RES_VAL_NUM_-4`  -4.758e+01  3.580e+01  -1.329    0.184
# `LAB_RES_VAL_NUM_-5`   5.087e+01  3.686e+01   1.380    0.168
# `LAB_RES_VAL_NUM_-6`  -3.904e+01  2.934e+01  -1.330    0.184
# `LAB_RES_VAL_NUM_-7`  -5.360e+00  2.491e+01  -0.215    0.830
# `LAB_RES_VAL_NUM_-8`   3.632e+00  3.995e+01   0.091    0.928
# `LAB_RES_VAL_NUM_-9`  -2.970e+01  4.829e+01  -0.615    0.539
# `LAB_RES_VAL_NUM_-10`  4.373e+01  3.176e+01   1.377    0.169
# ORDERING_DATE2_0       9.934e-01  4.644e-03 213.912   <2e-16 ***
#     `ORDERING_DATE2_-1`    6.938e-03  3.758e-02   0.185    0.854
# `ORDERING_DATE2_-2`   -4.759e-02  1.718e-01  -0.277    0.782
# `ORDERING_DATE2_-3`    4.001e-02  1.683e-01   0.238    0.812
# `ORDERING_DATE2_-4`    6.259e-04  3.875e-02   0.016    0.987
# `ORDERING_DATE2_-5`    2.662e-03  3.779e-02   0.070    0.944
# `ORDERING_DATE2_-6`   -6.027e-03  3.566e-02  -0.169    0.866
# `ORDERING_DATE2_-7`    7.174e-03  6.166e-02   0.116    0.907
# `ORDERING_DATE2_-8`   -6.529e-04  7.013e-02  -0.009    0.993
# `ORDERING_DATE2_-9`    1.849e-02  5.591e-02   0.331    0.741
# `ORDERING_DATE2_-10`  -1.015e+00  2.892e-02 -35.079   <2e-16 ***
#     ---
#     Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 57.87 on 1970 degrees of freedom
# Multiple R-squared:  0.9687,	Adjusted R-squared:  0.9683
# F-statistic:  2440 on 25 and 1970 DF,  p-value: < 2.2e-16

reg3$robse <- vcovHC(reg3, type = "HC1");
coeftest(reg3, reg3$robse);
threshold3_hat <- fitted(reg3); # predicted values
as.data.frame(threshold3_hat);
threshold3_resid <- residuals(reg3); # residuals
as.data.frame(threshold3_resid);
residualPlots(reg3);
avPlots(reg3, id.n = 2, id.cex = 0.6, col = "blue");

# Testing regressions
# reg_Test <- goodDataOrdered10DaysBeforeThreshold[sample(
#     nrow(goodDataOrdered10DaysBeforeThreshold[
#         goodDataOrdered10DaysBeforeThreshold$INT_FLAG>0,]), 100),];
reg_Test <- testset;
x3 <- predict(reg3, reg_Test, interval="prediction");
x3 <- as.data.frame(x3)
varx3 <- c("THRESHOLD_TIME", "LAB_RES_VAL_NUM_-5", "ORDERING_DATE2_-5", "INT_FLAG");
x3.1 <- reg_Test[varx3];
x3 <- cbind(x3.1, x3);
t3 <- x3$INT_FLAG + 22;
x3 <- as.data.frame(x3)
svg("Prediction_Kidney_LinearRegression.svg", width = 7, height = 7);
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, panel.first = grid(),
     col = alpha("blue", 1), bg = alpha("blue", .5),
     xlim = range(0:15),  ylim=range(-1:(max(x3$fit)+max(x3$upr))));
with(data = x3, expr = errbar(x3$THRESHOLD_TIME, x3$fit, fit + upr, fit - lwr,
                              bg = alpha("black", 0.1), errbar.col = alpha("black", 0.4),
                              pch = "", add = T, cap = 0.01));
dev.off();
plot(x3$THRESHOLD_TIME, x3$fit, pch = t3, col = alpha("blue", 1), bg = alpha("blue", .5), panel.first = grid());
}
