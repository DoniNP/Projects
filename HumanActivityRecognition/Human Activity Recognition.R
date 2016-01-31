library(AppliedPredictiveModeling)
library(caret)
library(randomForest)
library(ggplot2)

## Reading Data

trainData <- read.csv("pml-training.csv")
caseData <- read.csv("pml-testing.csv")

## Exploratory Data
dim(trainData)
table(trainData$classe)

## Cleaning Data
trainData <- trainData[, 6:dim(trainData)[2]]

#Categorizing columns that should be included, at least the data has 95% filled.
benchmark <- dim(trainData)[1] * 0.95
goodColumns <- !apply(trainData, 2, function(x) sum(is.na(x)) > treshold  || sum(x=="") > treshold)
trainData <- trainData[, goodColumns]

## Removing any near Zero Variance columns.
badColumns <- nearZeroVar(trainData, saveMetrics = TRUE)
trainData <- trainData[, badColumns$nzv==FALSE]

## Data Segmentation
set.seed(7777)
indexTraining <- createDataPartition(trainData$classe, p = 0.75)[[1]]
forTraining = trainData[indexTraining,]
forValidation = trainData[-indexTraining,]
  
## Trial With Prediction
set.seed(7777)
modelRF <- train(classe ~ . , data = forTraining, method = "rf")
modelGBM <- train(classe ~ . , data = forTraining, method = "gbm")
modelLDA <- train(classe ~ . , data = forTraining, method = "lda")

set.seed(7777)
predictionRF <- predict(modelRF, forValidation)
predictionGBM <- predict(modelGBM, forValidation)
predictionLDA <- predict(modelLDA, forValidation)

## Checking for Accuracy
confusionMatrix(forValidation$classe, predictionRF)
confusionMatrix(forValidation$classe, predictionGBM)
confusionMatrix(forValidation$classe, predictionLDA)

predictAnswer <- predict(modelRF, caseData)
predictAnswer




