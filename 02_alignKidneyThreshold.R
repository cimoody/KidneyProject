## 02_alignKidneyThreshold.R
## Written by Cristina Moody
## Aug 2015

# Installing Libraries and functions
# source(file = "00_librariesVNL.R"); # if starting R for 1st time today
# source(file = "01_getKidneyData_v2.R");


# Installing Gaussian Process Library
require(tgp);
# Installing package to compare the two data tables
require(compare);
require(plyr);
require(Hmisc);
# Package for classification
require(rpart);
require(rattle);
require(rpart.plot);
require(RColorBrewer);
require(randomForest);
require(party);
# Additional package for categorical graphics
require(vcd);

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

# FUNCTION TO CHANGE FACTOR TO NUMERIC
f2n <- function(val) {
    # FUNCTION TO CHANGE FACTOR TO NUMERIC
    val <- as.numeric(as.character(val));
}

# tID and THRESHOLD_VALUE for Creatinine, K, and P
{
    tID_CREAT = 1523; THRESHOLD_CREAT = 3;
    tID_K = 1520; THRESHOLD_K = 6;
    tID_P = 1555; THRESHOLD_P = 4; # Something is wrong here, the threshold is below the REFERENCE_HIGH
}

# Function to return the threshold value
getThreshold <- function(ListOfDataFrames){
    # Function to return the threshold value for the following labs
    tID_CREAT = 1523; THRESHOLD_CREAT = 3;
    tID_K = 1520; THRESHOLD_K = 6;
    tID_P = 1555; THRESHOLD_P = 4; # Something is wrong here, the threshold is below the REFERENCE_HIGH
    THRESHOLD_VALUE <- numeric(0);
    for (i in 1:length(ListOfDataFrames)){
        if (is.null(nrow(ListOfDataFrames[[i]]))){
            print(i);
            return("ERROR IN t0Finder, ListOfDataFrames has no rows!");
        }
        if (ListOfDataFrames[[i]]$LAB_COMP_CD[1] == tID_CREAT) {THRESHOLD_VALUE <- THRESHOLD_CREAT;}
        else if (ListOfDataFrames[[i]]$LAB_COMP_CD[1] == tID_K) {THRESHOLD_VALUE <- THRESHOLD_K;}
        else if (ListOfDataFrames[[i]]$LAB_COMP_CD[1] == tID_P) {THRESHOLD_VALUE <- THRESHOLD_P;}
        else {return("ERROR IN t0Finder, Lab does not have a THRESHOLD_VALUE!")}
    }
    return(THRESHOLD_VALUE);
}

# Subset into a list that only contains tests that start in a normal range.
normalStart <- function(ListOfDataFrames) {
    # Subset into a list that only contains tests that start in a normal range.
    x <- list();
    THRESHOLD_VALUE <- getThreshold(ListOfDataFrames)
    if (is.null(length(ListOfDataFrames)) ) {
        print("ERROR IN normalStart, ListOfDataFrames is not a list!");
        return(0);
    }
    for(i in 1:length(ListOfDataFrames)) {
        if ( ListOfDataFrames[[i]]$LAB_RES_VAL_NUM[1] < THRESHOLD_VALUE )
        {x[i] <- ListOfDataFrames[i];} else {x[i] <- NA}
    }
    x <- x[!is.na(x[])];
    return(x)
}

# Function to get choosen t0 (when test first crosses threshold values of Creat > 3, K > 6, P > 4)
# from each dataframe in list
t0Finder <- function(ListOfDataFrames){
    # Function to get choosen t0 (when test first crosses reference_high) from each dataframe in list
    t0list <- data.frame();
    for (i in 1:length(ListOfDataFrames)){
        if (is.null(nrow(ListOfDataFrames[[i]]))){
            print(i);
            return("ERROR IN t0Finder, ListOfDataFrames has no rows!");
        }
        THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
        for (j in 1:nrow(ListOfDataFrames[[i]])){
            if ( ListOfDataFrames[[i]]$LAB_RES_VAL_NUM[j] >= THRESHOLD_VALUE){
                t0list <- rbind(t0list,
                                data.frame(
                                    "STUDYID_ORDERTIME" = sprintf("%s_%s", ListOfDataFrames[[i]]$STUDYID[j],
                                                                ListOfDataFrames[[i]]$ORDERING_DATE[j]),
                                    "STUDYID" = ListOfDataFrames[[i]]$STUDYID[j],
                                    "encid" = ListOfDataFrames[[i]]$encid[j],
                                    "t0" = ListOfDataFrames[[i]]$ORDERING_DATE[j],
                                    "TEST_NUM" = j,
                                    "TOTAL_TESTS" = nrow(ListOfDataFrames[[i]]) ));
                break();
            }
        }
    }
    return((t0list));
}

# FUNCTION TO ADD PROPERTIME TO EACH DATAFRAME IN LIST
addProperTime <- function(ListOfDataFrames, t0DataFrame){
    # ADD PROPERTIME TO EACH DATAFRAME IN LIST
    for (i in 1:nrow(t0DataFrame)) {
        for (j in 1:length(ListOfDataFrames)){
            if ( ListOfDataFrames[[j]]$STUDYID[1]==t0DataFrame$STUDYID[i] ){
                ListOfDataFrames[[j]]$PROPER_TIME <-
                    ListOfDataFrames[[j]]$ORDERING_DATE -
                    t0DataFrame$t0[i];
            }
        }
    }
    return(ListOfDataFrames);
}

# Function to return a dataframe with the proper time #
returnProperTime <- function(originalListOfDataFrames) {
    # Return a dataframe with the proper time. ### Set to start uninteresting patients at t0 = 0 ###
    ListOfDataFrames <- normalStart(originalListOfDataFrames);
    if (length(ListOfDataFrames)==0) {print("No dataframes in list pass normalStart()"); return(0);}
    properTime <- t0Finder(ListOfDataFrames);
    if (length(properTime)==0) {
        adjustedList <- ListOfDataFrames;
    } else {
        adjustedList <- addProperTime(ListOfDataFrames, properTime);
    }
    for (j in 1:length(adjustedList)) {
        if (is.null(adjustedList[[j]]$PROPER_TIME)) {
            adjustedList[[j]]$PROPER_TIME <- adjustedList[[j]]$ORDERING_DATE -
                adjustedList[[j]]$ORDERING_DATE[which.min(adjustedList[[j]]$ORDERING_DATE)];
            # adjustedList[[j]]$ORDERING_DATE[1]; # ALigns not interesting patients to start at 0
            # Changing so that pateints that do not hit the threshold have data that ends before 0
            adjustedList[[j]]$INT_FLAG <- 0; # Adding INT_FLAG = 0 (not interesting)
        }
        else {
            adjustedList[[j]]$INT_FLAG <- 1;
        }
        adjustedList[[j]]$THRESHOLD_VALUE <- getThreshold(adjustedList[j]);
    }
    return(adjustedList);
}

## FUNCTION THAT PLOTS ALL STUDYID FOR ONE LAB FROM LIST RETURNED FROM getResultDateKIDSQL
# Use a list that was returned from example below
#   originalList <- getResultDateKIDSQL();
#   makePlot(originalList, 0.2);
makePlot <- function(ListOfDataFrames, shade) {
    # PLOTS ALL ECN_CSN_ID FOR ONE LAB FROM LIST RETURNED FROM getResultDateKIDSQL
    svg(sprintf("Lab_%s_gt_%g.svg", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS, width = 7, height = 5));
    ymax <- c(); ymin <- c(); xmax <- c(); xmin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) ));
        ymax[i] <- max(ymax[i], getThreshold(ListOfDataFrames));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) ));
        xmax[i] <- max(c(max(ListOfDataFrames[[i]][, 1][!is.na(ListOfDataFrames[[i]][, 1])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$ORDERING_DATE[1])) ));
        xmin[i] <- min(c(min(ListOfDataFrames[[i]][, 1][!is.na(ListOfDataFrames[[i]][, 1])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$ORDERING_DATE[1])) ));
    }
    co <- rainbow(length(ListOfDataFrames));
    plot(ListOfDataFrames[[i]]$ORDERING_DATE, ListOfDataFrames[[i]]$LAB_RES_VAL_NUM,
         bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
         type = "o", panel.first = grid(),
         xlim = range(min(xmin), max(xmax)),
         ylim = range(min(ymin), max(ymax)),
         xlab = "Time (hours)",
         ylab = sprintf("Lab %s  (%s)", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                        ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
    # Reference low
    abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
    # Reference high
    abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
    THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
    abline(h = THRESHOLD_VALUE, col = "dark blue");
    for (i in 1:length(ListOfDataFrames)){
        lines(ListOfDataFrames[[i]]$ORDERING_DATE, ListOfDataFrames[[i]]$LAB_RES_VAL_NUM,
              bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
              type = "b", panel.first = grid(),
              xlim = range(min(xmin), max(xmax)),
              ylim = range(min(ymin), max(ymax)),
              xlab = "Time (hours)",
              ylab = sprintf("Lab %s  (%s)", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                             ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
        THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
        abline(h = THRESHOLD_VALUE, col = "dark blue");
    }
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return(svg(sprintf("Lab_%s_gt_%g.svg", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                       ListOfDataFrames[[1]]$MIN_RAW_LABS, width = 7, height = 5)));
}

# Function to plot everything with new proper time
# Use a list that was returned from example below
#   originalList <- getResultDateKIDSQL();
#   List <- returnProperTime(originalList);
#   makePlot2(List, 0.2);
makePlot2 <- function(ListOfDataFrames, shade) {
    # Plot everything with new proper time
    svg(sprintf("Lab_by_hour_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$LAB_PX_CD[1], ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5);
    ymax <- c(); ymin <- c();
    xmax <- c(); xmin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) ));
        ymax[i] <- max(ymax[i], getThreshold(ListOfDataFrames));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) ));
        xmax[i] <- max(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
        xmin[i] <- min(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
    }
    co <- rainbow(length(ListOfDataFrames));
    if (length(ListOfDataFrames[[i]]$PROPER_TIME)==length(ListOfDataFrames[[i]]$LAB_RES_VAL_NUM)){
        plot(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$LAB_RES_VAL_NUM,
             bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
             type = "o", panel.first = grid(),
             xlim = range(min(xmin), max(xmax)),
             ylim = range(min(ymin), max(ymax)),
             xlab = "Time (hours)",
             ylab = sprintf("Lab %s  (%s)", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                            ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
        THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
        abline(h = THRESHOLD_VALUE, col = "dark blue");
        for (i in 1:length(ListOfDataFrames)){
            lines(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$LAB_RES_VAL_NUM,
                  bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
                  type = "b", panel.first = grid(),
                  xlim = range(min(xmin), max(xmax)),
                  ylim = range(min(ymin), max(ymax)),
                  xlab = "Time (hours)",
                  ylab = sprintf("Lab %s  (%s)", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                                 ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
            # Reference low
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
            # Reference high
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
            THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
            abline(h = THRESHOLD_VALUE, col = "dark blue");
        }
    }
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return( svg(sprintf("Lab_by_hour_%s_%s_gt_%g.svg",
                        ListOfDataFrames[[1]]$LAB_PX_CD[1], ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                        ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5) );
}

# Function to save each table to a csv file
makeCSV <- function(ListOfDataFrames) {
    # Function to save each table to a csv file
    for (j in 1:length(ListOfDataFrames)) {
        write.csv(ListOfDataFrames[[j]],
                  file = sprintf("E:/data_for_Oleg/Test__%s__patient_%s.csv",
                                 deparse(substitute(ListOfDataFrames)),
                                 ListOfDataFrames[[j]]$STUDYID[1]),
                  row.names = FALSE, na="");
    }
}

# Function to get the standard deviation, but return 0 if vector is of length 1.
getSD <- function(val) {
    # Function to get the standard deviation, but return 0 if vector is of length 1.
    if (is.na(sd(val))) {
        return(0);
    } else {
        return(sd(val));
    }
}

# Function to go through list of dataframes and
# replace repeated labs with mean and add standard deviation column
getMeanSDListDataFrames <- function(ListOfDataFrames) {
    # Function to go through list of dataframes and replace repeated labs with mean and add standard deviation column
    # Create new list so not to overwrite original
    newListOfDataFrames <- ListOfDataFrames;
    for (j in  1:length(ListOfDataFrames)){
        # Function to get the standard deviations of labs repeated on a given day
        sds <- aggregate(LAB_RES_VAL_NUM ~ ORDERING_DATE, data = ListOfDataFrames[[j]], FUN = getSD);
        # Function to get the mean value of labs repeated on a given day
        means <- aggregate(LAB_RES_VAL_NUM ~ ORDERING_DATE, data = ListOfDataFrames[[j]], FUN = mean);
        # Function that merges the hours and keeps the maximum LAB_RES_VAL_NUM - from the internet.
        newListOfDataFrames[[j]] <- do.call(rbind,
                                            lapply(split(newListOfDataFrames[[j]],
                                                         newListOfDataFrames[[j]]$ORDERING_DATE),
                                                   function(chunk) chunk[which.max(chunk$LAB_RES_VAL_NUM), ]));
        # Changing the maximum LAB_RES_VAL_NUM to the mean
        newListOfDataFrames[[j]]$LAB_RES_VAL_NUM <- means$LAB_RES_VAL_NUM;
        # Adding the standard deviation
        newListOfDataFrames[[j]]$SD_ORD_VAL <- sds$LAB_RES_VAL_NUM;
    }
    return(newListOfDataFrames);
}

# Function to plot everything with
# new mean for repeated labs and error bar of standard deviation
# Use a list that follow the below example
#   originalList <- getResultDateKIDSQL();
#   List <- returnProperTime(originalList);
#   ListMSD <- getMeanSDListDataFrames(List);
#   makePlot3(ListMSD, 0.2);
makePlot3 <- function(ListOfDataFrames, shade) {
    # Plot everything with new mean for repeated labs and error bar of standard deviation
    svg(sprintf("Mean_lab_by_hour_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$LAB_PX_CD[1], ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5);
    ymax <- c(); ymin <- c();
    xmax <- c(); xmin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]
                             + ListOfDataFrames[[i]][which.max(ListOfDataFrames[[i]][,2]),12], # Adds the error bar
                             as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) )));
        ymax[i] <- max(ymax[i], getThreshold(ListOfDataFrames));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]
                             - ListOfDataFrames[[i]][which.min(ListOfDataFrames[[i]][,2]),12], # Subtracts the error bar
                             as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) )));
        xmax[i] <- max(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
        xmin[i] <- min(as.numeric(ListOfDataFrames[[i]]$PROPER_TIME));
    }
    co <- rainbow(length(ListOfDataFrames));
    if (length(ListOfDataFrames[[i]]$PROPER_TIME)==length(ListOfDataFrames[[i]]$LAB_RES_VAL_NUM)){
        # Plot
        plot(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$LAB_RES_VAL_NUM,
             bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
             type = "o", panel.first = grid(),
             xlim = range(min(xmin), max(xmax)),
             ylim = range(min(ymin), max(ymax)),
             xlab = "Time (hours)",
             ylab = sprintf("Lab %s  (%s)", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                            ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Add Errorbars with SD_ORD_VAL
        with(data = ListOfDataFrames[[i]],
             expr = errbar(PROPER_TIME, LAB_RES_VAL_NUM, LAB_RES_VAL_NUM + SD_ORD_VAL,
                           LAB_RES_VAL_NUM - SD_ORD_VAL, pch = "", add = T,
                           errbar.col = alpha(co[i], shade+0.2), cap = 0.02));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
        THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
        abline(h = THRESHOLD_VALUE, col = "dark blue");
        for (i in 1:length(ListOfDataFrames)){
            lines(ListOfDataFrames[[i]]$PROPER_TIME, ListOfDataFrames[[i]]$LAB_RES_VAL_NUM,
                  bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
                  type = "b", panel.first = grid(),
                  xlim = range(min(xmin), max(xmax)),
                  ylim = range(min(ymin), max(ymax)),
                  xlab = "Time (hours)",
                  ylab = sprintf("Lab %s  (%s)", ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                                 ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
            # Add Errorbars with SD_ORD_VAL
            with(data = ListOfDataFrames[[i]],
                 expr = errbar(PROPER_TIME, LAB_RES_VAL_NUM, LAB_RES_VAL_NUM + SD_ORD_VAL,
                               LAB_RES_VAL_NUM - SD_ORD_VAL, pch = "", add = T,
                               errbar.col = alpha(co[i], shade+0.2), cap = 0.02));
            # Reference low
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
            # Reference high
            abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
            THRESHOLD_VALUE <- getThreshold(ListOfDataFrames);
            abline(h = THRESHOLD_VALUE, col = "dark blue");
        }
    }
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return( svg(sprintf("Mean_lab_by_hour_%s_%s_gt_%g.svg",
                        ListOfDataFrames[[1]]$LAB_PX_CD[1], ListOfDataFrames[[1]]$LAB_COMP_CD[1],
                        ListOfDataFrames[[1]]$MIN_RAW_LABS), width = 7, height = 5) );
}
