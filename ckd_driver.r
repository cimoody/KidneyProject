# Author: Dave Sanchez
library(data.table);
library(sqldf);

### Review the orders data
thisQuery  = "
SELECT
  LAB_PX_CD,
  LAB_PX_NM,
  COUNT(LAB_PX_CD)
AS Frequency FROM lab_orders_m
GROUP BY LAB_PX_CD
ORDER BY Frequency DESC";
thisResult = sqldf(thisQuery, dbname="CKD");
thisResult = as.data.table( thisResult );
thisResult[, Fraction := Frequency/sum(Frequency)];
thisResult[ Fraction > 0.01 ]

# Output:
# LAB_PX_CD                LAB_PX_NM Frequency   Fraction
# 1:     80048   BASIC METAB PANEL, BMP   1616444 0.08860972
# 2:      CBCD                 CBC/DIFF   1360792 0.07459547
# 3:  82962.05      GLUCOSE METER FLEXX   1182920 0.06484494
# 4:  85610.02                   PT/INR   1047126 0.05740102
# 5:     85027                      CBC   706090 0.03870622
# 6:     80061              LIPID PANEL   587638 0.03221295
# 7:  80053.01        COMPR METAB PANEL   563204 0.03087354
# 8:     84443                      TSH   445166 0.02440297
# 9:  82962.06 GLUCOSE METER STAT STRIP   350422 0.01920932
# 10:     85730                     APTT  307951 0.01688116
# 11:     83036          HEMOGLOBIN, A1C  295963 0.01622401
# 12:  85610.05        COAG CLINIC PTINR  284933 0.01561937
# 13:  84100.02               PHOSPHORUS  272029 0.01491200
# 14:     80076       HEP FUNCTION PANEL  263799 0.01446085
# 15:  82962.03            GLUCOSE METER  255065 0.01398207
# 16:  84460.03                      ALT  252716 0.01385331
# 17:     83735                MAGNESIUM  252400 0.01383598
# 18:     81002       ROUTINE URINALYSIS  243368 0.01334087
# 19:  83036.01           HEMOGLOBIN A1C  225508 0.01236183
# 20:  85018.01                      HGB  223346 0.01224331
# 21:      CKMB               CK WITH MB  200746 0.01100443
# 22:     84484              TROPONIN  T  198453 0.01087873

### Fix the lab data
# The labs data is only very lightly processed for the analyst's convenience.  For instance, LAB_COMP_ID
# is in the results, whereas the PROC_ID is in the orders.
# For convenience, we create a new SQLite database with some processed tables
db <- dbConnect(SQLite(), dbname="CKD") ;

# Join the necessary data.  We only care if the orderid is in both.
thisQuery="
SELECT
	t1.orderid,
  t1.STUDYID,
  t1.encid,
  t1.LAB_PX_CD,
  t2.LAB_COMP_CD,
  t2.LAB_RES_VAL_NUM,
  t2.LAB_RES_VAL_TXT,
  t2.LAB_ABN_FLAG,
  t1.lab_tkn_dt_m,
  t1.lab_tkn_tm
FROM
  lab_orders_m as t1
  INNER JOIN lab_results3_m as t2
    ON t1.orderid = t2.orderid";
thisResult = sqldf(thisQuery, dbname="CKD");
thisResult = as.data.table( thisResult );

