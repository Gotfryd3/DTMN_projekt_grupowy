#####
##  Basic info
#
# https://archive.ics.uci.edu/ml/datasets/Estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition+
# https://www.sciencedirect.com/science/article/pii/S2352340919306985?via%3Dihub


#####
##  Libraries
#
library(dplyr)
library(tidyverse)
library(caTools)
library(caret)
library(ROCR)
library(corrplot)

#####
##  Loading data
#
filename <- "ObesityDataSet_raw_and_data_sinthetic.csv"
rawData <- read.csv(filename)


#####
##  Data description
#
summary(rawData)

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

#####
##  Preparing data to logistic regression
#
preparedData <- subset(rawData, select = -c(Gender, NObeyesdad, family_history_with_overweight, SMOKE, CALC, CAEC, MTRANS, SCC, FAVC))
preparedData["Proper_weight"] <-  if_else(rawData$NObeyesdad == "Normal_Weight", 1, 0, missing = 0)
preparedData["Male"] <-  if_else(rawData$Gender == "Male", 1, 0, missing = 0)
preparedData["FamilyHistoryWithOverveight"] <-  if_else(rawData$family_history_with_overweight == "yes", 1, 0, missing = 0)
preparedData["CaloriesConsumptionMonitirong"] <-  if_else(rawData$SCC == "yes", 1, 0, missing = 0)
preparedData["FrequencyOfConsumptionHighCaloriesFood"] <-  if_else(rawData$FAVC == "yes", 1, 0, missing = 0)
preparedData["CIGARETES"] <-  if_else(rawData$SMOKE == "yes", 1, 0, missing = 0)
preparedData["ConsumptionOfAlcohol"] <-  if_else(rawData$CALC == "Frequently", 2, 
                                                 if_else(rawData$CALC == "Sometimes", 1, 0, missing = 0), missing = 0)
preparedData["ConsumptionOfFoofBetweenMeals"] <-  if_else(rawData$CAEC == "Always", 3, 
                                                          if_else(rawData$CAEC == "Frequently", 2, 
                                                                  if_else(rawData$CAEC == "Sometimes", 1, 0, missing = 0), missing = 0), missing = 0)
preparedData["COMMUTE"] <-  if_else(rawData$MTRANS == "Automobile", 3, 
                                    if_else(rawData$MTRANS == "Motorbike", 2, 
                                            if_else(rawData$MTRANS == "Public_Transportation", 1, 0, missing = 0), missing = 0), missing = 0)

#####
##  Type definition of variables
#
dataset <- as.data.frame(lapply(preparedData[c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE", "Proper_weight", "Male", "FamilyHistoryWithOverveight", 
                                               "CaloriesConsumptionMonitirong", "FrequencyOfConsumptionHighCaloriesFood", "CIGARETES", "ConsumptionOfAlcohol", 
                                               "ConsumptionOfFoofBetweenMeals", "COMMUTE")], as.numeric))

#####
##  Data analise
#
summary(dataset)
print(dataset)
correlationMatrix <- cor(dataset, method = "pearson")
colnames(correlationMatrix) <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE", "Proper_weight", "Male", "FHWO", "CCM", "FOCHCF", "CIGARETES", "COA", "COFBM", "COMMUTE")
rownames(correlationMatrix) <- c("Age", "Height", "Weight", "FCVC", "NCP", "CH2O", "FAF", "TUE", "Proper_weight", "Male", "FHWO", "CCM", "FOCHCF", "CIGARETES", "COA", "COFBM", "COMMUTE")
corrplot(correlationMatrix)

#####
##  Data partition
#
logicClassTrue <- data.frame(filter(dataset, Proper_weight == 1))
logicClassFalse <- data.frame(filter(dataset, Proper_weight == 0))

split = sample.split(logicClassTrue, SplitRatio = 0.7)
trainDataT = subset(logicClassTrue, split == TRUE)
testDataT  = subset(logicClassTrue, split == FALSE)

split = sample.split(logicClassFalse, SplitRatio = 0.7)
trainDataF = subset(logicClassFalse, split == TRUE)
testDataF  = subset(logicClassFalse, split == FALSE)

trainData <- rbind(trainDataT, trainDataF)
testData <- rbind(testDataT, testDataF)


#####
##  First model - simple test
#
formula <- Proper_weight ~ Age + Height + Weight + FCVC + NCP + CH2O + FAF + TUE + Male + FamilyHistoryWithOverveight + CaloriesConsumptionMonitirong + FrequencyOfConsumptionHighCaloriesFood + CIGARETES + ConsumptionOfAlcohol + ConsumptionOfFoofBetweenMeals + COMMUTE
model1 <- glm(formula, family=quasibinomial(link="logit"), as.data.frame(trainData))
summary(model1)
prediction1 <- predict(model1, testData)
pred <- ifelse(prediction1 > 0.5, 1, 0)
table(testData$Proper_weight, pred)

#####
##  Diagnostics plots of model no. 1
#
plot(residuals.glm(model1, type="response"))

#####
##  Reduction of input variables using correlation plot and test p-val
#
formula2 <- Proper_weight ~ Weight + FCVC + NCP + CH2O + FAF + TUE + Male + FrequencyOfConsumptionHighCaloriesFood + CIGARETES + ConsumptionOfFoofBetweenMeals + COMMUTE
model2 <- glm(formula2, family=binomial(link="logit"), as.data.frame(trainData))
summary(model2)
prediction2 <- predict(model2, testData)
pred2 <- ifelse(prediction2 > 0.5, 1, 0)
table(testData$Proper_weight, pred2)

#####
##  Influence variables removal
#
cookeDistance <- cooks.distance(model2)
plot(cookeDistance)
influential <- as.numeric(names(sort(cookeDistance, decreasing = TRUE)[1:10]))
trainDataI <- trainData[-influential,]

#####
##  Reduction of input observations using Cooke distance
#
formula3 <- Proper_weight ~ Weight + FCVC + NCP + CH2O + FAF + TUE + Male + FrequencyOfConsumptionHighCaloriesFood + CIGARETES + ConsumptionOfFoofBetweenMeals + COMMUTE
model3 <- glm(formula3, family=binomial(link="logit"), as.data.frame(trainDataI))
summary(model3)
prediction3 <- predict(model2, testData)
pred3 <- ifelse(prediction3 > 0.5, 1, 0)
table(testData$Proper_weight, pred3)

#####
##  Reduction of input variables using test p-val
#
formula4 <- Proper_weight ~ Weight + FCVC + FAF + TUE + Male + FrequencyOfConsumptionHighCaloriesFood + CIGARETES + ConsumptionOfFoofBetweenMeals + COMMUTE
model4 <- glm(formula4, family=binomial(link="logit"), as.data.frame(trainDataI))
summary(model4)
prediction4 <- predict(model4, testData)
pred4 <- ifelse(prediction4 > 0.5, 1, 0)
table(testData$Proper_weight, pred4)

#####
##  Diagnostics plots of model no. 4
#
plot(residuals.glm(model4, type="response"))

#####
##  Commentary section
#
# Liczba zmiennych została ograniczona z 17 do 9. Mimo to performacne modelu wzrósł - mniej błedów 2-giego rodzaju.