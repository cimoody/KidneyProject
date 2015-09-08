# KidneyProject
### Code for project to predict when a patient will require dialysis

**TO USE**

1. source('00_librariesVNL.R')
2. Change the data directory dDir to the location of your data in 
    * 01\_getKidneyData\_v2.R, line 16
    * 02_alignKidneyThreshold.R, line 29
    * 03\_Kidney\_kNNClassifier\_LinearReg.R, line 25
3. source('01\_getKidneyData_v2.R') to get functions for making the lists of dataframes.
    * To remake and save the lists I used in my analysis, set makeLists <- 1; on line 180.
    * To change which fields from the SQL database are saved in the lists, adjust labQuery on line 24. **Be sure to add the column throughout the entire code (Look for STUDYID to find where it needs to be added).**
4. source('02\_alignKedneyThreshold.R') to get the functions used for retrieving and storing the aligned time. I've called this time frame PROPER_TIME in the code.
5. source('03\_Kidney\_kNNClassifier\_LinearReg.R') to get the functions for creating the dataframes for the 'meta'-patient. 
    * getTimeTrainMatrix(originalListOfDataFrames, random, HoursPerTimeStep) creates a dataframe with each STUDYID as an entry and only has 10 times steps before the Threshold (or for STUDYID without a threshold crossing, the maximum lab value - use random = 0 for this alignment). Set the length of the times steps with HoursPerTimeStep. HoursPerTimeStep = 1 corresponds to 1 hour time intervals. 
    * getTrainMatrix(originalListOfDataFrames) creates a dataframe with each lab measurement as an entry. Useful for finding where the labs are the most dense in time.
    * getPopData() retreives the specified non-lab data per STUDYID from the SQLite database using the popQuery1 and popQuery2 queries.
    *mergedDF on lines 403 & 405 merges the results of each STUDYID population data with the lab results from getTimeTrainMatrix().
    * The function createMeta() on line 430 and used on line 510 cleans up the merged dataframe into a useable format for the classifier and regression tools in R.
    * The function INTkNNclassifier(metaBDF, percentTrain, flag.cut = 0.5) takes the entire 'meta'-patient dataframe from createMeta(), randomly splits it into a training set and a testing set with percentTrain (must be a number < 1) and creates a classifier using kknn in R. The flag.cut is the division on the prediction (need a flag of 0 or 1). The function returns a 2 element list: 1st element is the dataframe with a predicted flag PrINT\_FLAG column; 2nd element is the kknn classifier made over the training set (use summary() or str() to get information on the classifier).
    * The function RegKid(metaBDF, percentTrain, OnlyINT1) creates a multiple linear regression model over the dataframe percentTrain of metaBDF. The flag OnlyINT1 can be set to 1 to only perform the regression model over training entries with PrINT\_FLAG = 1. Setting OnlyINT1=0 performs the regression over training entries with both PrINT\_FLAG = 1 & PrINT\_FLAG = 0.
    * The remainder of the code is a test of a regression created with RegKid(). My current method of visually testing the regression model is to look for a linear trend when plotting the actual THRESHOLD_TIME against the fit from RegKid(). This can be seen in lines 828-844.
     