# 01_getKidneyData_v2.R
# Aug 2015
# by Cristina Moody

# Installing Libraries
# source(file = "00_librariesVNL.R"); # if starting R for 1st time today

# Load libraries
require(data.table);
require(lubridate);
require(sqldf);

# Working directory: "C:/Users/CMoody/Desktop/workspace/VNL"
wDir <- sprintf("%s%s", getwd(), "/");
# Data directory: E:/VNL Data from Joe
dDir <- "E:/KidneyData/";
# SQL database path and name
dbfile <- paste(dDir, "CKD", sep = "");
# Access CKD SQL database
db <- dbConnect(SQLite(), dbname = dbfile);
# remember to call db <- dbDisconnect(db) when finished!

# Query for fields in the lab_orders_m.csv and lab_results3_m.csv files
labQuery="
    SELECT
t1.orderid,
t1.STUDYID,
t1.encid,
t1.LAB_PX_CD,
t2.LAB_COMP_CD,
t2.LAB_RES_VAL_NUM,
t2.LAB_RES_UNIT,
t2.REFERENCE_LOW,
t2.REFERENCE_HIGH,
t1.lab_tkn_dt_m,
t1.lab_tkn_tm
FROM
lab_orders_m as t1
INNER JOIN lab_results3_m as t2
ON t1.orderid = t2.orderid";


# Retreive the necessary data from the SQL database
getDF <- function(dbfile, thisQuery){
    # Join the necessary data.  We only care if the orderid is in both.
    # Must have the SQL database already connected with dbConnect. This function
    # returns a dataframe from the SQL database dbfile using the information in thisQuery
    thisResult = sqldf(thisQuery, dbname = dbfile);
    thisResult = as.data.table( thisResult );
}

numQuery = "
    SELECT
STUDYID,
LAB_COMP_CD,
COUNT(LAB_COMP_CD)
AS Frequency FROM lab_results_m
GROUP BY LAB_COMP_CD
ORDER BY Frequency DESC";

Query1  = "
SELECT
STUDYID,
LAB_PX_CD,
LAB_PX_NM,
COUNT(LAB_PX_CD)
AS Frequency FROM lab_orders_m
GROUP BY LAB_PX_CD
ORDER BY Frequency DESC";

Query2  = "
SELECT
STUDYID,
LAB_COMP_CD,
COUNT(LAB_COMP_CD)
AS Frequency FROM lab_results3_m
GROUP BY LAB_COMP_CD
ORDER BY Frequency DESC";
getNumLabs <- function(dbfile, thisQuery){
    # Must have the SQL database already connected with dbConnect. This function
    # returns a dataframe listing the frequencies of each lab from the SQL database dbfile
    # using the information in thisQuery
    thisResult = sqldf(thisQuery, dbname = dbfile);
    thisResult = as.data.table( thisResult );
    thisResult[, Fraction := Frequency/sum(Frequency)];
    thisResult[ Fraction > 0.01 ]
}

cutoff <- 20; # Setting the minimum number of labs per patient
tID <- 1523; # Creatinine COMPONENT_ID for lab_results3_m query
# tID <- 1520; # K COMPONENT_ID for lab_results3_m query
# tID <- 1555; # P COMPONENT_ID for the lab_results3_m query
{
    tID_CREAT = 1523; THRESHOLD_CREAT = 3;
    tID_K = 1520; THRESHOLD_K = 6;
    tID_P = 1555; THRESHOLD_P = 4; # Something is wrong here, the threshold is below the REFERENCE_HIGH
}

getResultDateKIDSQL <- function(dbfile, query, cutoff, tID){
    # returns a list of dataframes, one for each patient, for labs identified with tID that have at least
    # the cutoff of repetitions of that test. The information is selected from the SQLite database dbfile
    # with query.
    goodlab <- getDF(dbfile, query);
    labnames <- paste(goodlab$STUDYID, goodlab$LAB_COMP_CD, sep = "_");
    # Use table to get the frequency of each combined patient & lab
    topLabs <- table(labnames);
    # Order these by frequency
    df <- topLabs;
    df <- df[order(df)];
    # cutoff <- 50;
    dfNotRare <- df[df>=cutoff];
    # Determining which labs pass cutoff <= 50;
    # Accessing the row names in the table (since they are ID_lab)
    rn <- row.names(dfNotRare);
    rn <- strsplit(rn, "_"); # Splitting up the names into the components
    # Transposing the list rn
    lis <- rn;
    m <- do.call(rbind, lis);
    split(m, col(m));
    rn2 <- m;
    gtCOtests <- data.frame(rn2); # Creating a data frame for ease of use
    # Combining the number of tests with the patient and lab information
    gtCOtests <- cbind(gtCOtests, dfNotRare);
    colnames(gtCOtests) <- c("STUDYID", "LAB_COMP_CD", "TIMES_TEST_REPEATED");
    # Finding which tests patients (determined by STUDYID) had more than cutoff # of times
    whTestsgtCO <- table(gtCOtests$LAB_COMP_CD);
    whTestsgtCO <- whTestsgtCO[order(whTestsgtCO)];
    # STUDYID from
    firstENC <- gtCOtests[gtCOtests$LAB_COMP_CD==as.numeric(tID),];
    # STUDYID of pateintw with specific test
    v <- firstENC$STUDYID; # Vector of STUDYID want to plot
    # Function to get specific STUDYID lab results for specific lab
    getPat <- function(vec, tID. = tID) {
        pat <- goodlab[(goodlab$LAB_COMP_CD==as.numeric(tID.) &
                            goodlab$STUDYID==vec),];
        return(pat);
    }
    # Getting lab results for specific lab
    pat <- list(lapply(v, FUN = getPat));
    # Isolatinge the ORDERING_DATE
    trTimeLAB <- list(c(length(v)));
    for (i in 1:length(v)){
        trTimeLAB[[i]] <- pat[[1]][i][[1]]$lab_tkn_dt_m * 24 + pat[[1]][i][[1]]$lab_tkn_tm;
        # trTimeLAB[[i]] <- as.Date(pat[[1]][i][[1]]$ORDERING_DATE,format = "%m/%d/%Y %H:%M");
    }
    # Isolating lab LAB_RES_VAL_NUM
    patLAB <- list(c(length(v)));
    for (i in 1:length(v)){
        patLAB[[i]] <- as.numeric(as.character(pat[[1]][i][[1]]$LAB_RES_VAL_NUM));
    }
    # Creating tables for each STUDYID of ORDERING_DATE & LAB_RES_VAL_NUM in a list
    patTLAB <- list(c(length(v)));
    for (i in 1:length(v)){
        patTLAB[[i]] <- data.frame(Test_Order_Date = trTimeLAB[i], LAB = patLAB[i],
                                   pat[[1]][i][[1]]$LAB_RES_UNIT,
                                   pat[[1]][i][[1]]$STUDYID,
                                   pat[[1]][i][[1]]$orderid,
                                   pat[[1]][i][[1]]$encid,
                                   pat[[1]][i][[1]]$REFERENCE_HIGH,
                                   pat[[1]][i][[1]]$REFERENCE_LOW,
                                   pat[[1]][i][[1]]$LAB_COMP_CD,
                                   pat[[1]][i][[1]]$LAB_PX_CD,
                                   MIN_RAW_LABS = cutoff);
        names(patTLAB[[i]]) <- c("ORDERING_DATE", "LAB_RES_VAL_NUM", "LAB_RES_UNIT",
                                 "STUDYID", "orderid", "encid", "REFERENCE_HIGH", "REFERENCE_LOW",
                                 "LAB_COMP_CD", "LAB_PX_CD", "MIN_RAW_LABS");
    }
    # Removing invalid results of 99999
    for (i in 1:length(v)){
        patTLAB[[i]] <- patTLAB[[i]][patTLAB[[i]]$LAB_RES_VAL_NUM < 77777, ];
    }
    # Ordering chronologically
    for (i in 1:length(v)){
        patTLAB[[i]] <- patTLAB[[i]][order(patTLAB[[i]]$ORDERING_DATE), ];
    }
    return(patTLAB);
}

makeLists <- 0; # It takes a while to remake all the lists, so I would recommend only making the ones you need
if (makeLists) { # Code to create lists of dataframes. Set makeLists <- 1 to run.
    CREAT_1523_gt20 <- getResultDateKIDSQL(dbfile, labQuery, cutoff = 20, tID = tID_CREAT);
    K_1520_gt20 <- getResultDateKIDSQL(dbfile, labQuery, cutoff = 20, tID = tID_K);
    P_1555_gt20 <- getResultDateKIDSQL(dbfile, labQuery, cutoff = 20, tID = tID_P);
}
