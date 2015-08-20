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
}
if (!init){
    listcontents <-  list.files(path = dDir,  pattern = "_1.rda", include.dirs = T);
    for (i in 1:length(listcontents)) {
        load(file = sprintf("%s%s", dDir, listcontents[i]));
    }
}


# # Files
# listfiles <- list.files(pattern = "\\.csv");
# listfiles_endx2 <- list.files(path = dDir_dx2);
# listfiles_enm <- list.files(path = dDir_enm);
# listfiles_l.ord <- list.files(path = dDir_l.ord);
# listfiles_l.rslt <- list.files(path = dDir_l.rslt);
# listfiles_m.ord <- list.files(path = dDir_m.ord);
# listfiles_vit2 <- list.files(path = dDir_vit2);
# # file names
# filenames <- vapply(listfiles, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# filenames_endx2 <- vapply(listfiles_endx2, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# filenames_enm <- vapply(listfiles_enm, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# filenames_l.ord <- vapply(listfiles_l.ord, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# filenames_l.rslt <- vapply(listfiles_l.rslt, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# filenames_m.ord <- vapply(listfiles_m.ord, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# filenames_vit2 <- vapply(listfiles_vit2, FUN = function(x){sub(".csv", "", x)}, FUN.VALUE = c(""));
# # Reading in the files.
# # Had to split listfiles[6,7,9,10,11,14] because they were too large to read in
# init <- 0; # Variable to set to read and write data into R .rda files.
# if (init){ # Loading files into R
#     nulls <- c("<null>", "NA", "<NA>");
#     ckd_lab_results_m       <- read.csv(file = listfiles[1],  header = T, stringsAsFactors = F, na.strings = nulls);
#     ckd_lab_results_m_v1.1  <- read.csv(file = listfiles[2],  header = T, stringsAsFactors = F, na.strings = nulls);
#     ckd_patients_m          <- read.csv(file = listfiles[3],  header = T, stringsAsFactors = F, na.strings = nulls);
#     CKD_pt_link             <- read.csv(file = listfiles[4],  header = T, stringsAsFactors = F, na.strings = nulls);
#     egfr_m                  <- read.csv(file = listfiles[5],  header = T, stringsAsFactors = F, na.strings = nulls);
#     # encounters_dx2_m        <- read.csv(file = listfiles[6],  header = T, stringsAsFactors = F, na.strings = nulls);
#     # encounters_m            <- read.csv(file = listfiles[7],  header = T, stringsAsFactors = F, na.strings = nulls);
#     esrd_patients_m         <- read.csv(file = listfiles[8],  header = T, stringsAsFactors = F, na.strings = nulls);
#     # lab_orders_m            <- read.csv(file = listfiles[9],  header = T, stringsAsFactors = F, na.strings = nulls);
#     # lab_results3_m          <- read.csv(file = listfiles[10], header = T, stringsAsFactors = F, na.strings = nulls);
#     # medication_orders2_m    <- read.csv(file = listfiles[11], header = T, stringsAsFactors = F, na.strings = nulls);
#     problem_list_m          <- read.csv(file = listfiles[12], header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals1_m               <- read.csv(file = listfiles[13], header = T, stringsAsFactors = F, na.strings = nulls);
#     # vitals2_m               <- read.csv(file = listfiles[14], header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_1      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[1], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_2      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[2], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_3      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[3], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_4      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[4], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_5      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[5], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_6      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[6], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_7      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[7], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_8      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[8], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_9      <- read.csv(file = paste(dDir_dx2, listfiles_endx2[9], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_10     <- read.csv(file = paste(dDir_dx2, listfiles_endx2[10], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_11     <- read.csv(file = paste(dDir_dx2, listfiles_endx2[11], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_12     <- read.csv(file = paste(dDir_dx2, listfiles_endx2[12], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_13     <- read.csv(file = paste(dDir_dx2, listfiles_endx2[13], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_dx2_m_14     <- read.csv(file = paste(dDir_dx2, listfiles_endx2[14], sep = ""),
#                                         header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_1      <- read.csv(file = paste(dDir_enm, listfiles_enm[1], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_10     <- read.csv(file = paste(dDir_enm, listfiles_enm[2], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_11     <- read.csv(file = paste(dDir_enm, listfiles_enm[3], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_12     <- read.csv(file = paste(dDir_enm, listfiles_enm[4], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_13     <- read.csv(file = paste(dDir_enm, listfiles_enm[5], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_2      <- read.csv(file = paste(dDir_enm, listfiles_enm[6], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_3      <- read.csv(file = paste(dDir_enm, listfiles_enm[7], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_4      <- read.csv(file = paste(dDir_enm, listfiles_enm[8], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_5      <- read.csv(file = paste(dDir_enm, listfiles_enm[9], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_6      <- read.csv(file = paste(dDir_enm, listfiles_enm[10], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_7      <- read.csv(file = paste(dDir_enm, listfiles_enm[11], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_8      <- read.csv(file = paste(dDir_enm, listfiles_enm[12], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     encounters_m_9      <- read.csv(file = paste(dDir_enm, listfiles_enm[13], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_1      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[1], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_2      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[2], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_3      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[3], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_4      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[4], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_5      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[5], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_6      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[6], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_7      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[7], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_orders_m_8      <- read.csv(file = paste(dDir_l.ord, listfiles_l.ord[8], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_1    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[1], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_10   <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[2], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_11   <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[3], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_12   <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[4], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_2    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[5], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_3    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[6], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_4    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[7], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_5    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[8], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_6    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[9], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_7    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[10], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_8    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[11], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     lab_results3_m_9    <- read.csv(file = paste(dDir_l.rslt, listfiles_l.rslt[12], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     medication_orders2_m_1    <- read.csv(file = paste(dDir_m.ord, listfiles_m.ord[1], sep = ""),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
#     medication_orders2_m_2    <- read.csv(file = paste(dDir_m.ord, listfiles_m.ord[2], sep = ""),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
#     medication_orders2_m_3    <- read.csv(file = paste(dDir_m.ord, listfiles_m.ord[3], sep = ""),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
#     medication_orders2_m_4    <- read.csv(file = paste(dDir_m.ord, listfiles_m.ord[4], sep = ""),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
#     medication_orders2_m_5    <- read.csv(file = paste(dDir_m.ord, listfiles_m.ord[5], sep = ""),
#                                           header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_1         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[1], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_10        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[2], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_11        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[3], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_12        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[4], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_13        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[5], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_14        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[6], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_15        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[7], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_16        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[8], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_17        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[9], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_18        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[10], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_19        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[11], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_2         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[12], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_20        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[13], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_21        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[14], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_22        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[15], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_23        <- read.csv(file = paste(dDir_vit2, listfiles_vit2[16], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_3         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[17], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_4         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[18], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_5         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[19], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_6         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[20], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_7         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[21], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_8         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[22], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#     vitals2_m_9         <- read.csv(file = paste(dDir_vit2, listfiles_vit2[23], sep = ""),
#                                     header = T, stringsAsFactors = F, na.strings = nulls);
#
# }
# # Saving data frames. Will use these from now on.
# if (init){ # Saving files in .rda format
#     save(ckd_lab_results_m,      file = sprintf("%s%s%s", dDir, filenames[1],  ".rda"));
#     save(ckd_lab_results_m_v1.1, file = sprintf("%s%s%s", dDir, filenames[2],  ".rda"));
#     save(ckd_patients_m,         file = sprintf("%s%s%s", dDir, filenames[3],  ".rda"));
#     save(CKD_pt_link,            file = sprintf("%s%s%s", dDir, filenames[4],  ".rda"));
#     save(egfr_m,                 file = sprintf("%s%s%s", dDir, filenames[5],  ".rda"));
#     # save(encounters_dx2_m,       file = sprintf("%s%s%s", dDir, filenames[6],  ".rda"));
#     # save(encounters_m,           file = sprintf("%s%s%s", dDir, filenames[7],  ".rda"));
#     save(esrd_patients_m,        file = sprintf("%s%s%s", dDir, filenames[8],  ".rda"));
#     # save(lab_orders_m,           file = sprintf("%s%s%s", dDir, filenames[9],  ".rda"));
#     # save(lab_results3_m,         file = sprintf("%s%s%s", dDir, filenames[10], ".rda"));
#     # save(medication_orders2_m,   file = sprintf("%s%s%s", dDir, filenames[11], ".rda"));
#     save(problem_list_m,         file = sprintf("%s%s%s", dDir, filenames[12], ".rda"));
#     save(vitals1_m,              file = sprintf("%s%s%s", dDir, filenames[13], ".rda"));
#     # (vitals2_m,              file = sprintf("%s%s%s", dDir, listfiles[14], ".rda"));
#     save(encounters_dx2_m_1,     file = sprintf("%s%s%s", dDir, filenames_endx2[1],  ".rda"));
#     save(encounters_dx2_m_10,    file = sprintf("%s%s%s", dDir, filenames_endx2[2],  ".rda"));
#     save(encounters_dx2_m_11,    file = sprintf("%s%s%s", dDir, filenames_endx2[3],  ".rda"));
#     save(encounters_dx2_m_12,    file = sprintf("%s%s%s", dDir, filenames_endx2[4],  ".rda"));
#     save(encounters_dx2_m_13,    file = sprintf("%s%s%s", dDir, filenames_endx2[5],  ".rda"));
#     save(encounters_dx2_m_14,    file = sprintf("%s%s%s", dDir, filenames_endx2[6],  ".rda"));
#     save(encounters_dx2_m_2,     file = sprintf("%s%s%s", dDir, filenames_endx2[7],  ".rda"));
#     save(encounters_dx2_m_3,     file = sprintf("%s%s%s", dDir, filenames_endx2[8],  ".rda"));
#     save(encounters_dx2_m_4,     file = sprintf("%s%s%s", dDir, filenames_endx2[9],  ".rda"));
#     save(encounters_dx2_m_5,     file = sprintf("%s%s%s", dDir, filenames_endx2[10],  ".rda"));
#     save(encounters_dx2_m_6,     file = sprintf("%s%s%s", dDir, filenames_endx2[11],  ".rda"));
#     save(encounters_dx2_m_7,     file = sprintf("%s%s%s", dDir, filenames_endx2[12],  ".rda"));
#     save(encounters_dx2_m_8,     file = sprintf("%s%s%s", dDir, filenames_endx2[13],  ".rda"));
#     save(encounters_dx2_m_9,     file = sprintf("%s%s%s", dDir, filenames_endx2[14],  ".rda"));
#     #
#     save(encounters_m_1,  file = sprintf("%s%s%s", dDir, filenames_enm[1],  ".rda"));
#     save(encounters_m_10, file = sprintf("%s%s%s", dDir, filenames_enm[2],  ".rda"));
#     save(encounters_m_11, file = sprintf("%s%s%s", dDir, filenames_enm[3],  ".rda"));
#     save(encounters_m_12, file = sprintf("%s%s%s", dDir, filenames_enm[4],  ".rda"));
#     save(encounters_m_13, file = sprintf("%s%s%s", dDir, filenames_enm[5],  ".rda"));
#     save(encounters_m_2,  file = sprintf("%s%s%s", dDir, filenames_enm[6],  ".rda"));
#     save(encounters_m_3,  file = sprintf("%s%s%s", dDir, filenames_enm[7],  ".rda"));
#     save(encounters_m_4,  file = sprintf("%s%s%s", dDir, filenames_enm[8],  ".rda"));
#     save(encounters_m_5,  file = sprintf("%s%s%s", dDir, filenames_enm[9],  ".rda"));
#     save(encounters_m_6,  file = sprintf("%s%s%s", dDir, filenames_enm[10],  ".rda"));
#     save(encounters_m_7,  file = sprintf("%s%s%s", dDir, filenames_enm[11],  ".rda"));
#     save(encounters_m_8,  file = sprintf("%s%s%s", dDir, filenames_enm[12],  ".rda"));
#     save(encounters_m_9,  file = sprintf("%s%s%s", dDir, filenames_enm[13],  ".rda"));
# }
# Create SQL to hold data since it uses too much memory to load in data frames.
# {
# setwd(dDir);
# db <- dbConnect(SQLite(), dbname = "ckd_lab_results");
# dbindex <- colnames(ckd_lab_results_m);
# dbWriteTable(conn = db, name = "ckd_lab_results", value = ckd_lab_results_m);
# dbListTables(db); # Shows that "ckd_lab_results" is in the database
# dbListFields(db, "ckd_lab_results"); # lists columns in "ckd_lab_results"
# dbGetQuery(db, "SELECT * from ckd_lab_results");  # Access data in ckd_lab_results
# stID <- dbGetQuery(db, "SELECT STUDYID from ckd_lab_results")
# dbDisconnect(db)
# # Create directly from csv files
# sqldf("attach 'ckd.sqlite' as new");
# read.csv.sql(sprintf("%s%s", wDir, "/ckd_lab_results_m.csv"),
#              sql = "create table ckd_lab_results as select * from file", dbname = "ckd.sqlite");
#
#
# # # Example from http://stackoverflow.com/a/4335739/419994
# # # create a test file
# # write.table(iris, "iris.csv", sep = ",", quote = FALSE, row.names = FALSE)
# #
# # # create an empty database.
# # # can skip this step if database already exists.
# # sqldf("attach testingdb as new")
# # # or: cat(file = "testingdb")
# #
# # # read into table called iris in the testingdb sqlite database
# # library(sqldf)
# # read.csv.sql("iris.csv", sql = "create table main.iris as select * from file",
# #              dbname = "testingdb")
# #
# # # look at first three lines
# # sqldf("select * from main.iris limit 3", dbname = "testingdb")
# }
