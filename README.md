# KidneyProject
### Code for project to predict when a patient will require dialysis

**TO USE**

1. source('00_librariesVNL.R')
2. Change the data directory dDir to the location of your data in 
    * 01\_getKidneyData\_v2.R, line 16
    * 02_alignKidneyThreshold.R, line 29
    * 03\_Kidney\_kNNClassifier_LinearReg.R, line 25
3. source('01\_getKidneyData_v2.R') to get functions for making the lists of dataframes.
    * To remake and save the lists I used in my analysis, set makeLists <- 1; on line 180.
    * To change which fields from the SQL database are saved in the lists, adjust labQuery on line 24. *Be sure to add the column throughout the entire code (Look for STUDYID to find where it needs to be added)*
4. source('02\_alignKedneyThreshold.R') to get the functions used for retrieving the aligned time. I've called this time frame PROPER_TIME in the code
5. source