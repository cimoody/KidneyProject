# Author: Dave Sanchez
#
# Moves the CKD data into a SQLite database

# Load libraries
library(data.table);
library(lubridate);
library(sqldf);

## Connect to DB
db <- dbConnect(SQLite(), dbname="CKD")


############################## encounters_m.csv ##############################
# Prepare table
dbRemoveTable(conn= db, name = "encounters_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/encounters_m.csv", header=TRUE, nrows=chunkSize, skip=0, verbose=TRUE);
column.names = colnames(thisTable);

# loop through rest of chunks
while( nrow(thisTable) > 2 ) {
  # Fix enc_csn_id_m
  thisTable[, enc_csn_id_m := as.integer( gsub("enc", "", enc_csn_id_m ))];

  # Fix date
  refDate = ymd("1900-01-01");
  thisTable[, enc_dt_m := as.numeric( dmy( enc_dt_m ) - refDate ) ];

  thisSkip = thisSkip + chunkSize;
  dbWriteTable(conn = db, name = "encounters_m", value = thisTable, row.names = FALSE, append=TRUE );

  thisTable = fread("data/ckd/encounters_m.csv", header=FALSE, nrows=chunkSize, skip=thisSkip);
  setnames(thisTable, colnames(thisTable), column.names);
}

# Fix enc_csn_id_m
thisTable[, enc_csn_id_m := as.integer( gsub("enc", "", enc_csn_id_m ))];

# Fix date
refDate = ymd("1900-01-01");
thisTable[, enc_dt_m := as.numeric( dmy( enc_dt_m ) - refDate ) ];

# Final write
dbWriteTable(conn = db, name = "encounters_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");


############################## ckd_lab_results_m_v1.1.csv ##############################
# Prepare table
dbRemoveTable(conn= db, name = "lab_results_m");

# Initial read
thisTable = fread("data/ckd/ckd_lab_results_m_v1.2.csv", header=TRUE, verbose=TRUE, na.strings = c("NA", "N/A", "", "<null>"));

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, lab_res_dt_m := as.numeric( ymd(lab_res_dt_m) - refDate )];
thisTable = thisTable[ complete.cases( lab_res_dt_m ) ];

# Fix orderid
thisTable[, orderid := as.integer( gsub("lab", "", orderid) ) ];

# Write to database
dbWriteTable(conn = db, name = "lab_results_m", value = thisTable, row.names = FALSE );
remove("thisTable");


############################## ckd_patients_m.csv ##############################
# Prepare table
dbRemoveTable(conn= db, name = "patients_m");

# Initial read
thisTable = fread("data/ckd/ckd_patients_m.csv", header=TRUE, verbose=TRUE, na.strings = c("NA", "N/A", "", "<null>"));

# Fix dates and times
refDate = ymd("1900-01-01");
thisTable[, M_FIRST_EGFR_DT := as.numeric(dmy(M_FIRST_EGFR_DT) - refDate)];
thisTable[, M_BIRTH_DATE := as.numeric(dmy(M_BIRTH_DATE) - refDate)];
thisTable[, M_NEXT_EGFR_DT := as.numeric(dmy(M_NEXT_EGFR_DT) - refDate)];
thisTable[, M_DEATH_DATE := as.numeric(dmy( M_DEATH_DATE ) - refDate)];

thisTable[, FIRST_EGFR_TM := hour( hms(FIRST_EGFR_TM) ) + minute( hms(FIRST_EGFR_TM) )/60];
thisTable[, NEXT_EGFR_TM := hour(  hms(NEXT_EGFR_TM) ) + minute( hms(NEXT_EGFR_TM) )/60];

# Add a new age column
thisTable[, AGE := as.numeric( gsub(">","", M_AGE )) ];

# Write to DB
dbWriteTable(conn = db, name = "patients_m", value = thisTable, row.names = FALSE );
remove("thisTable");


############################### CKD_pt_link.csv ################################
# Prepare table
dbRemoveTable(conn= db, name = "pt_link");

# Initial read
thisTable = fread("data/ckd/CKD_pt_link.csv", header=TRUE, verbose=TRUE);

## I DO NOT THINK THIS FILE SHOULD BE HERE


################################## egfr_m.csv ##################################
# Prepare table
dbRemoveTable(conn= db, name = "egfr_m");

# Initial read
thisTable = fread("data/ckd/egfr_m.csv", header=TRUE, verbose=TRUE, na.strings = c("NA", "N/A", "", "<null>"));

# Cast ORDER to int
thisTable[, ORDER_MODE := ifelse( ORDER_MODE == "Inpatient", 1, 0)];
setnames(thisTable, "ORDER_MODE", "INPATIENT");

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, M_EGFR_DT := as.numeric(dmy(M_EGFR_DT) - refDate)];
thisTable[, EGFR_TM   := hour( hms(EGFR_TM) ) + minute( hms(EGFR_TM) )/60 ];

# Write to database
dbWriteTable(conn = db, name = "egfr_m", value = thisTable, row.names = FALSE );
remove("thisTable");

############################# encounters_dx2_m.csv #############################
# Prepare table
dbRemoveTable(conn= db, name = "encounters_dx2_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/encounters_dx2_m.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));
column.names = colnames(thisTable);

# loop through rest of chunks
while( nrow(thisTable) > 2 ) {
  # Fix encounter numbers
  thisTable[, enc_csn_id_m := as.integer( gsub("enc", "", enc_csn_id_m ))];

  thisSkip = thisSkip + chunkSize;
  dbWriteTable(conn = db, name = "encounters_dx2_m", value = thisTable, row.names = FALSE, append=TRUE );

  thisTable = fread("data/ckd/encounters_dx2_m.csv", header=FALSE, nrows=chunkSize, skip=thisSkip);
  setnames(thisTable, colnames(thisTable), column.names);

}

# Fix encounter numbers
thisTable[, enc_csn_id_m := as.integer( gsub("enc", "", enc_csn_id_m ))];


# Final write
dbWriteTable(conn = db, name = "encounters_dx2_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");


############################# esrd_patients_m.csv ##############################
# Prepare table
dbRemoveTable(conn= db, name = "esrd_patients_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/esrd_patients_m.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));

# Create new kind of age
thisTable[, AGE := as.numeric( gsub(">","", M_AGE) )];

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, M_BIRTH_DATE := as.numeric(dmy(M_BIRTH_DATE) - refDate)];
thisTable[, M_DEATH_DATE := as.numeric(dmy(M_DEATH_DATE) - refDate)];
thisTable[, M_FIRST_EGFR_L_15 := as.numeric(dmy(M_FIRST_EGFR_L_15) - refDate)];
thisTable[, M_USRDS_ESRD_DATE := as.numeric(dmy(M_USRDS_ESRD_DATE) - refDate)];

# Write to database
dbWriteTable(conn = db, name = "esrd_patients_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");


############################### lab_orders_m.csv ###############################
# Prepare table
dbRemoveTable(conn= db, name = "lab_orders_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/lab_orders_m.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));
column.names = colnames(thisTable);


# loop through rest of chunks
while( nrow(thisTable) > 2 ) {
  # Fix encounter, order numbers
  thisTable[, encid := as.integer( gsub("enc", "", encid ))];
  thisTable[, orderid := as.integer( gsub("lab", "", orderid ))];

  # Fix dates
  refDate = ymd("1900-01-01");
  thisTable[, lab_ord_dt_m := as.numeric( dmy( lab_ord_dt_m ) - refDate ) ];
  thisTable[, lab_tkn_dt_m := as.numeric( dmy( lab_tkn_dt_m ) - refDate ) ];
  thisTable[, lab_res_dt_m := as.numeric( dmy( lab_res_dt_m ) - refDate ) ];

  # Fix times
  thisTable[, lab_ord_tm := hour( hms(lab_ord_tm) ) - minute( hms(lab_ord_tm) )/60 ];
  thisTable[, lab_tkn_tm := hour( hms(lab_tkn_tm) ) - minute( hms(lab_tkn_tm) )/60 ];

  thisSkip = thisSkip + chunkSize;
  dbWriteTable(conn = db, name = "lab_orders_m", value = thisTable, row.names = FALSE, append=TRUE );

  thisTable = fread("data/ckd/lab_orders_m.csv", header=FALSE, nrows=chunkSize, skip=thisSkip);
  setnames(thisTable, colnames(thisTable), column.names);

}

# Fix encounter, order numbers
thisTable[, encid := as.integer( gsub("enc", "", encid ))];
thisTable[, orderid := as.integer( gsub("lab", "", orderid ))];

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, lab_ord_dt_m := as.numeric( dmy( lab_ord_dt_m ) - refDate ) ];
thisTable[, lab_tkn_dt_m := as.numeric( dmy( lab_tkn_dt_m ) - refDate ) ];
thisTable[, lab_res_dt_m := as.numeric( dmy( lab_res_dt_m ) - refDate ) ];

# Fix times
thisTable[, lab_ord_tm := hour( hms(lab_ord_tm) ) - minute( hms(lab_ord_tm) )/60 ];
thisTable[, lab_tkn_tm := hour( hms(lab_tkn_tm) ) - minute( hms(lab_tkn_tm) )/60 ];

# Final write
dbWriteTable(conn = db, name = "lab_orders_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");


############################## lab_results3_m.csv ##############################   FIXME
# Prepare table
dbRemoveTable(conn= db, name = "lab_results3_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/lab_results3_m_v1.1.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));
column.names = colnames(thisTable);

# Fix lab orderid
thisTable[, orderid := as.integer( gsub("lab", "", orderid ))];

# Fix date
refDate = ymd("1900-01-01");
thisTable[, lab_res_dt_m := as.numeric( dmy(lab_res_dt_m) - refDate) ];

# Final write
dbWriteTable(conn = db, name = "lab_results3_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");

########################### medication_orders2_m.csv ###########################
# Prepare table
dbRemoveTable(conn= db, name = "medication_orders2_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/medication_orders2_m.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));
column.names = colnames(thisTable);

# loop through rest of chunks
while( nrow(thisTable) > 2 ) {
  # Fix encounter, med order numbers
  thisTable[, encid := as.integer( gsub("enc", "", encid ))];
  thisTable[, medordid := as.integer( gsub("med", "", medordid ))];

  # Fix dates
  refDate = ymd("1900-01-01");
  thisTable[, med_ord_dt_m := as.numeric( dmy( med_ord_dt_m ) - refDate ) ];
  thisTable[, med_start_dt_m := as.numeric( dmy( med_start_dt_m ) - refDate ) ];
  thisTable[, med_end_dt_m := as.numeric( dmy( med_end_dt_m ) - refDate ) ];

  thisSkip = thisSkip + chunkSize;
  dbWriteTable(conn = db, name = "medication_orders2_m", value = thisTable, row.names = FALSE, append=TRUE );

  thisTable = fread("data/ckd/medication_orders2_m.csv", header=FALSE, nrows=chunkSize, skip=thisSkip);
  setnames(thisTable, colnames(thisTable), column.names);

}

# Fix encounter, med order numbers
thisTable[, encid := as.integer( gsub("enc", "", encid ))];
thisTable[, medordid := as.integer( gsub("med", "", medordid ))];

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, med_ord_dt_m := as.numeric( dmy( med_ord_dt_m ) - refDate ) ];
thisTable[, med_start_dt_m := as.numeric( dmy( med_start_dt_m ) - refDate ) ];
thisTable[, med_end_dt_m := as.numeric( dmy( med_end_dt_m ) - refDate ) ];

# Final write
dbWriteTable(conn = db, name = "medication_orders2_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");


############################## problem_list_m.csv ##############################
# Prepare table
dbRemoveTable(conn= db, name = "problem_list_m");

# Initial read
thisTable = fread("data/ckd/problem_list_m.csv", header=TRUE, verbose=TRUE, na.strings = c("NA", "N/A", "", "<null>"));

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, prob_entry_dt_m := as.numeric( dmy( prob_entry_dt_m ) - refDate ) ];
thisTable[, prob_noted_dt_m := as.numeric( dmy( prob_noted_dt_m ) - refDate ) ];
thisTable[, prob_resolved_dt_m := as.numeric( dmy( prob_resolved_dt_m ) - refDate ) ];

# Write to database
dbWriteTable(conn = db, name = "problem_list_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");


################################ vitals1_m.csv #################################
# Prepare table
dbRemoveTable(conn= db, name = "vitals1_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/vitals1_m.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));
column.names = colnames(thisTable);

# loop through rest of chunks
while( nrow(thisTable) > 2 ) {
  # Fix encid
  thisTable[, encid := as.integer( gsub("enc","",encid) ) ];

  # Fix dates
  refDate = ymd("1900-01-01");
  thisTable[, vtl_dt_m := as.numeric( dmy( vtl_dt_m ) - refDate ) ];
  thisTable[, vtl_tm := hms( vtl_tm )/hours(1) ];

  thisSkip = thisSkip + chunkSize;
  dbWriteTable(conn = db, name = "vitals1_m", value = thisTable, row.names = FALSE, append=TRUE );

  thisTable = fread("data/ckd/vitals1_m", header=FALSE, nrows=chunkSize, skip=thisSkip);
  setnames(thisTable, colnames(thisTable), column.names);

}

# Fix encid
thisTable[, encid := as.integer( gsub("enc","",encid) ) ];

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, vtl_dt_m := as.numeric( dmy( vtl_dt_m ) - refDate ) ];
thisTable[, vtl_tm := hms( vtl_tm )/hours(1) ];

# Write to database
dbWriteTable(conn = db, name = "vitals1_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");

################################ vitals2_m.csv #################################
# Prepare table
dbRemoveTable(conn= db, name = "vitals2_m");

# Read file in chunks
chunkSize = 5000000;
thisSkip = 0;

# Initial read
thisTable = fread("data/ckd/vitals2_m.csv", header=TRUE, verbose=TRUE, nrows=chunkSize, skip=thisSkip, na.strings = c("NA", "N/A", "", "<null>"));
column.names = colnames(thisTable);

# loop through rest of chunks
while( nrow(thisTable) > 2 ) {
  # Fix encounter, fsid
  thisTable[, encid := as.integer( gsub("enc", "", encid ))];
  thisTable[, fsid := as.integer( gsub("fs", "", fsid ))];

  # Fix dates
  refDate = ymd("1900-01-01");
  thisTable[, vtl_dt_m := as.numeric( dmy( vtl_dt_m ) - refDate ) ];

  thisSkip = thisSkip + chunkSize;
  dbWriteTable(conn = db, name = "vitals2_m", value = thisTable, row.names = FALSE, append=TRUE );

  thisTable = fread("data/ckd/vitals2_m.csv", header=FALSE, nrows=chunkSize, skip=thisSkip);
  setnames(thisTable, colnames(thisTable), column.names);

}

# Fix encounter, fsid
thisTable[, encid := as.integer( gsub("enc", "", encid ))];
thisTable[, fsid := as.integer( gsub("fs", "", fsid ))];

# Fix dates
refDate = ymd("1900-01-01");
thisTable[, vtl_dt_m := as.numeric( dmy( vtl_dt_m ) - refDate ) ];

# Final write
dbWriteTable(conn = db, name = "vitals2_m", value = thisTable, row.names = FALSE, append=TRUE );
remove("thisTable");
