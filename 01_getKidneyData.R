## 01_getKidneyData.R

## Kidney Data
## Written by Cristina Moody
## July 2015

# Installing Libraries
# source(file = "librariesVNL.R"); # If starting R for 1st time, else do below

# Installing package to compare the two data tables
require(compare);
require(plyr);
require(Hmisc);
require(scales);
require(XLConnect);
require(sqldf);
# Directories:
wDir <- getwd();
dDir <- "E:/KidneyData/"; # E:\KidneyData
lab_comp_table <- read.csv("lab_comp_id_name.csv", header = T, stringsAsFactors = F); # codes to lab name table
lab_code <- paste(lab_comp_table$LAB_COMP_NM, lab_comp_table$LAB_COMP_CD, sep = " = ");
lab_code <- as.data.frame(table(lab_code));
View(lab_code);
listDirs <- list.dirs(recursive = F);
listDirs <- listDirs[3:16]; # 1st two were "./.git"  & "./.Rproj.user"
# Files
listfiles_1stPiece <- list.files(dir(pattern = "\\_Pieces", include.dirs = T),
                                 pattern = "_1.csv", include.dirs = T);
# Names
filenames_1stPiece <- vapply(listfiles_1stPiece, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# File names with path to open
listfiles <- c(numeric(0));
for (i in 1:length(listDirs)){
    listDirs[i] <- substring(listDirs[i], 3);
    listfiles[i] <- paste(wDir, listDirs[i], listfiles_1stPiece[i], sep = "/");
}
# Reading in the files.
# Had to split files, 1stPiece (_1) only contains the first 950000 lines of each file
init <- 0; # Variable to set to read and write data into R .rda files.
if (init){ # Loading files into R
    nulls <- c("<null>", "NA", "<NA>", "");
    ckd_lab_results_m_1       <- read.csv(file = listfiles[1],  header = T, stringsAsFactors = F, na.strings = nulls);
    ckd_lab_results_m_v1.1_1  <- read.csv(file = listfiles[2],  header = T, stringsAsFactors = F, na.strings = nulls);
    ckd_patients_m_1          <- read.csv(file = listfiles[3],  header = T, stringsAsFactors = F, na.strings = nulls);
    CKD_pt_link_1             <- read.csv(file = listfiles[4],  header = T, stringsAsFactors = F, na.strings = nulls);
    egfr_m_1                  <- read.csv(file = listfiles[5],  header = T, stringsAsFactors = F, na.strings = nulls);
    encounters_dx2_m_1        <- read.csv(file = listfiles[6],  header = T, stringsAsFactors = F, na.strings = nulls);
    encounters_m_1            <- read.csv(file = listfiles[7],  header = T, stringsAsFactors = F, na.strings = nulls);
    esrd_patients_m_1         <- read.csv(file = listfiles[8],  header = T, stringsAsFactors = F, na.strings = nulls);
    lab_orders_m_1            <- read.csv(file = listfiles[9],  header = T, stringsAsFactors = F, na.strings = nulls);
    lab_results3_m_1          <- read.csv(file = listfiles[10], header = T, stringsAsFactors = F, na.strings = nulls);
    medication_orders2_m_1    <- read.csv(file = listfiles[11], header = T, stringsAsFactors = F, na.strings = nulls);
    problem_list_m_1          <- read.csv(file = listfiles[12], header = T, stringsAsFactors = F, na.strings = nulls);
    vitals1_m_1               <- read.csv(file = listfiles[13], header = T, stringsAsFactors = F, na.strings = nulls);
    vitals2_m_1               <- read.csv(file = listfiles[14], header = T, stringsAsFactors = F, na.strings = nulls);
    # Full files
#     lab_orders_m              <- read.csv(paste(wDir, "lab_orders_m.csv", sep = "/"),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m            <- read.csv(paste(wDir, "lab_results3_m.csv", sep = "/"),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
}
# Saving data frames. Will use these from now on.
if (init){ # Saving files in .rda format
    save(ckd_lab_results_m_1,      file = sprintf("%s%s%s", dDir, filenames_1stPiece[1],  ".rda"));
    save(ckd_lab_results_m_v1.1_1, file = sprintf("%s%s%s", dDir, filenames_1stPiece[2],  ".rda"));
    save(ckd_patients_m_1,         file = sprintf("%s%s%s", dDir, filenames_1stPiece[3],  ".rda"));
    save(CKD_pt_link_1,            file = sprintf("%s%s%s", dDir, filenames_1stPiece[4],  ".rda"));
    save(egfr_m_1,                 file = sprintf("%s%s%s", dDir, filenames_1stPiece[5],  ".rda"));
    save(encounters_dx2_m_1,       file = sprintf("%s%s%s", dDir, filenames_1stPiece[6],  ".rda"));
    save(encounters_m_1,           file = sprintf("%s%s%s", dDir, filenames_1stPiece[7],  ".rda"));
    save(esrd_patients_m_1,        file = sprintf("%s%s%s", dDir, filenames_1stPiece[8],  ".rda"));
    save(lab_orders_m_1,           file = sprintf("%s%s%s", dDir, filenames_1stPiece[9],  ".rda"));
    save(lab_results3_m_1,         file = sprintf("%s%s%s", dDir, filenames_1stPiece[10], ".rda"));
    save(medication_orders2_m_1,   file = sprintf("%s%s%s", dDir, filenames_1stPiece[11], ".rda"));
    save(problem_list_m_1,         file = sprintf("%s%s%s", dDir, filenames_1stPiece[12], ".rda"));
    save(vitals1_m_1,              file = sprintf("%s%s%s", dDir, filenames_1stPiece[13], ".rda"));
    save(vitals2_m_1,              file = sprintf("%s%s%s", dDir, filenames_1stPiece[14], ".rda"));
#     save(lab_orders_m,             file = sprintf("%s%s",   dDir, "lab_orders_m.rda"));
#     save(lab_results3_m,           file = sprintf("%s%s",   dDir, "lab_results3_m.rda"));
}
if (!init){
    listcontents <-  list.files(path = dDir,  pattern = "_1.rda", include.dirs = T);
    for (i in 1:length(listcontents)) {
        load(file = sprintf("%s%s", dDir, listcontents[i]));
    }
}

# Checking columns for all NA columns:
if( sum( is.na(ckd_lab_results_m_1$LAB_RES_VAL_NUM2) + 0 ) >=
    length( ckd_lab_results_m_1$LAB_RES_VAL_NUM2 ) ) {
    print(" LAB_RES_VAL_NUM2 is all NA\n");
} else
{
    print( sum( is.na(ckd_lab_results_m_1$LAB_RES_VAL_NUM2)+0));
    print(" of");
    print(length( ckd_lab_results_m_1$LAB_RES_VAL_NUM2 ));
    print(" LAB_RES_VAL_NUM2 is NA." );
}

# Subsetting data, columns to keep
ckd_lab_results_m_1_cols <- c("STUDYID", "orderid", "LAB_COMP_CD", "LAB_RES_VAL_NUM", "LAB_RES_UNIT",
                              "lab_tkn_dt_m", "REFERENCE_LOW", "REFERENCE_HIGH");
ckd_lab_results_m_v1.1_1_cols <- c("STUDYID", "orderid", "LAB_COMP_CD", "LAB_RES_VAL_NUM", "LAB_RES_UNIT",
                                   "lab_res_dt_m", "REFERENCE_LOW", "REFERENCE_HIGH");
lab_orders_m_1_cols <- c("STUDYID", "orderid", "lab_tkn_dt_m", "lab_tkn_tm", "PROC_ID",
                         "LAB_PX_CD");
lab_results3_m_1_cols <- c("STUDYID", "orderid", "LAB_COMP_CD", "LAB_RES_VAL_NUM", "LAB_RES_UNIT",
                           "lab_res_dt_m", "REFERENCE_LOW", "REFERENCE_HIGH");
removeChar <- function(df, colnam = "orderid", chars.rm.num = 3){
    # Function to remove specific number of characters (indicted by chars.rm.num)
    # from the front of a value in a column (colnam) in a dataframe (df)
    replcol <- c(colnam);
    replvec <- df[[replcol]];
    col1 <- vapply(replvec, FUN = function(x){substring(x, 0, chars.rm.num)}, FUN.VALUE = c(""));
    if (as.numeric(table(col1))!=nrow(df)) {
        print("ERROR!!  ERROR!! CANNOT USE removeChar");
        print(table(col1)); print(nrow(df)); print(colnam); print(names(df));
    }
    col2 <- vapply(replvec, FUN = function(x){substring(x, chars.rm.num+1)}, FUN.VALUE = c(""));
    col2 <- as.numeric(col2);
    df[[replcol]] <- col2;
    return(df);
}
fixDates <- function(df, colnam){
    replcol <- c(colnam);
    replvec <- df[[replcol]];
    if (grepl("/", replvec[1])){
        replvec <- mdy(replvec, tz = "EST");
    } else {
        replvec <- dmy(replvec, tz = "EST");
    }
    df[[replcol]] <- replvec;
    return(df);
}
# Subsetting data and saving
if (init){
    # Subsetting the columns
    ckd_lab_results_m_1_sub <- ckd_lab_results_m_1[ckd_lab_results_m_1_cols];
    ckd_lab_results_m_v1.1_1_sub <- ckd_lab_results_m_v1.1_1[ckd_lab_results_m_v1.1_1_cols];
    lab_orders_m_1_sub <- lab_orders_m_1[lab_orders_m_1_cols];
    lab_results3_m_1_sub <- lab_results3_m_1[lab_results3_m_1_cols];
    # removing the extra characters on the otherwise numeric column orderid
    ckd_lab_results_m_1_sub <- removeChar(ckd_lab_results_m_1_sub);
    ckd_lab_results_m_v1.1_1_sub <- removeChar(ckd_lab_results_m_v1.1_1_sub);
    lab_orders_m_1_sub <- removeChar(lab_orders_m_1_sub);
    lab_results3_m_1_sub <- removeChar(lab_results3_m_1_sub);
    # Changing the date columns from characters to Date POSIXct values
    ckd_lab_results_m_1_sub <- fixDates(ckd_lab_results_m_1_sub, "lab_tkn_dt_m");
    ckd_lab_results_m_v1.1_1_sub <- fixDates(ckd_lab_results_m_v1.1_1_sub, "lab_res_dt_m");
    lab_orders_m_1_sub <- fixDates(lab_orders_m_1_sub, "lab_tkn_dt_m");
    lab_results3_m_1_sub <- fixDates(lab_results3_m_1_sub, "lab_res_dt_m");
    # Saving cleaned up data frames
    save(ckd_lab_results_m_1_sub, ckd_lab_results_m_v1.1_1_sub,
         lab_orders_m_1_sub, lab_results3_m_1_sub,
         file = sprintf("%s%s", dDir, "Subsets_ckd_labPiece1.rda"));
}
# Loading subsetted data
if (!init){
    load(file = sprintf("%s%s", dDir, "Subsets_ckd_labPiece1.rda"));
}

getResultDateKID <- function(df, cutoff, tID){
    # RETURNS LAB RESULT AND ORDERING DATA FOR ANY GIVEN LAB from dataframe df
    # Set which lab to examine with tID to LAB_COMP_CD (numeric). See lab_code to find number.
    # Set the minimum number of raw labs with cutoff (actual number may be less due to invalid results or multiplies per day)
    # Make a variable that is a combination of STUDYID, orderid, LAB_COMP_CD
    # CPT_CODE & LAB_COMP_CD are the lab
    goodlab <- df;
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
    gtCOtests$LAB_COMP_CD = paste(gtCOtests$CPT_CODE, gtCOtests$LAB_COMP_CD, sep = "_");
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
        trTimeLAB[[i]] <- as.Date(pat[[1]][i][[1]]$ORDERING_DATE,
                                  format = "%m/%d/%Y %H:%M");
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
                                   pat[[1]][i][[1]]$REFERENCE_HIGH,
                                   pat[[1]][i][[1]]$REFERENCE_LOW,
                                   pat[[1]][i][[1]]$LAB_COMP_CD,
                                   MIN_RAW_LABS = cutoff);
        names(patTLAB[[i]]) <- c("ORDERING_DATE", "LAB_RES_VAL_NUM", "LAB_RES_UNIT",
                                 "STUDYID", "orderid", "REFERENCE_HIGH", "REFERENCE_LOW",
                                 "LAB_COMP_CD", "MIN_RAW_LABS");
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



## FUNCTION THAT PLOTS ALL ECN_CSN_ID FOR ONE LAB FROM LIST RETURNED FROM getResultDate
makePlot <- function(ListOfDataFrames, shade) {
    # PLOTS ALL ECN_CSN_ID FOR ONE LAB FROM LIST RETURNED FROM getResultDate
    svg(sprintf("Lab_%s_%s_gt_%g.svg",
                ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                ListOfDataFrames[[1]]$MIN_RAW_LABS, width = 7, height = 5));
    ymax <- c(); ymin <- c();
    for(i in 1:length(ListOfDataFrames)){
        ymax[i] <- max(c(max(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])) ));
        ymin[i] <- min(c(min(ListOfDataFrames[[i]][, 2][!is.na(ListOfDataFrames[[i]][, 2])]),
                         as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])) ));
    }
    co <- rainbow(length(ListOfDataFrames));
    plot(ListOfDataFrames[[i]]$ORDERING_DATE, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
         bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
         type = "o", panel.first = grid(),
         xlim = range(as.Date("2008-02-01"),as.Date("2008-11-30")),
         ylim = range(min(ymin), max(ymax)),
         xlab = "Time (days)",
         ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                        ListOfDataFrames[[1]]$COMPONENT_ID[1],
                        ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
    # Reference low
    abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_LOW[1])), col = "red");
    # Reference high
    abline(h = as.numeric(as.character(ListOfDataFrames[[1]]$REFERENCE_HIGH[1])), col = "red");
    axis.Date(side = 1, x = as.Date(ListOfDataFrames[[1]]$ORDERING_DATE));
    for (i in 1:length(ListOfDataFrames)){
        lines(ListOfDataFrames[[i]]$ORDERING_DATE, ListOfDataFrames[[i]]$ORD_NUM_VALUE,
              bg = alpha(co[i], shade), col = alpha(co[i], shade+0.1), pch = 21,
              type = "b", panel.first = grid(),
              xlim = range(as.Date("2008-02-01"),as.Date("2008-11-30")),
              ylim = range(min(ymin), max(ymax)),
              xlab = "Time (days)",
              ylab = sprintf("Lab %s_%s   (%s)", ListOfDataFrames[[1]]$CPT_CODE[1],
                             ListOfDataFrames[[1]]$COMPONENT_ID[1],
                             ListOfDataFrames[[1]]$REFERENCE_UNIT[1]));
        # Reference low
        abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_LOW[1])), col = "red");
        # Reference high
        abline(h = as.numeric(as.character(ListOfDataFrames[[i]]$REFERENCE_HIGH[1])), col = "red");
        axis.Date(side = 1, x = as.Date(ListOfDataFrames[[i]]$ORDERING_DATE));}
    nm <-deparse(substitute(ListOfDataFrames));
    print(nm);
    dev.off();
    return(svg(sprintf("Lab_%s_%s_gt_%g.svg",
                       ListOfDataFrames[[1]]$CPT_CODE[1], ListOfDataFrames[[1]]$COMPONENT_ID[1],
                       ListOfDataFrames[[1]]$MIN_RAW_LABS, width = 7, height = 5)));
}
