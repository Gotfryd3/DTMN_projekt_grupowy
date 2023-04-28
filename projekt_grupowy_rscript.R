#####
##  Basic info
#
#https://archive.ics.uci.edu/ml/datasets/Estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition+


#####
##  Libraries
#
library(dplyr)

#####
##  Loading data
#
filename <- "ObesityDataSet_raw_and_data_sinthetic.csv"
rawData <- read.csv(filename)

#####
##  Type definition of variables
#
rawDataset <- cbind(as.data.frame(lapply(rawData[c("Gender", "family_history_with_overweight", "FAVC", "CAEC", "SMOKE", "SCC", "CALC", "MTRANS", "NObeyesdad")], as.factor)), 
                    as.data.frame(lapply(rawData[c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE")], as.numeric)))

#####
##  Data description
#
summary(rawDataset)

# FCVC - frequency of consumption of vegetables
# CAEC - consumption of food between meals
# SCC - calories consumption monitoring
# CALC - consumption of alcohol
# MTRANS - transportation used
# FAVC - frequency of consumption high caloric food
# NCP - number of main meals
# CH2O - daily consumption of water
# FAF - physical activity frequency
# TUE - time using technology devices
# NObeyesdad - type

