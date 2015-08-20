## Libraries to install and source (so it is done once) for VNL and KidneyProject
install.packages(c("plyr", "stringi", "ggplot2"));
library(plyr);
library(stringi);
library(ggplot2);
# Installing Gaussian Process Library
install.packages("tgp");
library(tgp);

# Installing package to compare the two data tables
install.packages("compare");
library(compare);
install.packages("Hmisc", dependencies = T);
library(Hmisc);

# Package for classification
library(rpart);
install.packages('rattle');
library(rattle);
install.packages('rpart.plot');
library(rpart.plot);
# install.packages('RColorBrewer');
library(RColorBrewer);
install.packages('randomForest');
library(randomForest);
install.packages('party');
library(party);
install.packages("RWeka", dependencies = TRUE);
install.packages("rJava");
library(rJava);
library(RWeka);

# Additional package for categorical graphics
install.packages("vcd");
library(vcd);

library(lmtest);
library(sandwich);
install.packages("quantreg");
library(quantreg);
library(car);
library(zoo);
library(scales);
library(igraph);
install.packages("UsingR");
library(UsingR);

# Package for dealing with dates
install.packages("lubridate");
library(lubridate);

# SQL
install.packages("sqldf");
install.packages("XLConnect");
library(sqldf);
library(XLConnect);
library(DBI);
