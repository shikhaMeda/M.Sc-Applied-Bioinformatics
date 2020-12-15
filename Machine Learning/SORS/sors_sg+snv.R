library(caret)
library(factoextra)
require(tidyr)
require(tidyverse)
library(splitstackshape)
library(tidyverse)
require("dplyr")
library(randomForest)
library(e1071)
library(kernlab)
require( prospectr) # savitzkyGolay
library(magrittr)

#Prepare dataset for analysis
DATA <- read.csv("SORS_av.csv", row.names = 1, header = T)   
category<-read.csv("honey_origin_m.csv", row.names=1, header = T)

category<-category[-32,]
Data_category<-cbind(DATA,category)

Data_floral.origin<-Data_category[,-c(1026:1027)]
floral.origin<-as.factor(Data_floral.origin$Floral.origin)

Fl.1<-as.factor(Data_floral.origin$Floral.origin)
sors_spectra<-Data_floral.origin[,-1025]

##smooth SORS data
sors_spectra <- savitzkyGolay(DATA, p = 7, w = 9, m = 0)
# SNV
sors_spectra <- t(sors_spectra)                                            # transpose spectra
sors_spectra <- scale(sors_spectra, center = T, scale = T)                      # snv on spectra
sors_spectra <- t(sors_spectra)

sors_Data<-cbind(sors_spectra,Fl.1)
sors_Data<-as.data.frame(sors_Data)

# convert factors to numbers
sors_Data$Fl.1 %<>% factor
sors_Data$Fl.1 <- as.numeric(sors_Data$Fl.1)

# mutate bell and ling as one class
sors_Data <- sors_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 3, 1))
sors_Data <- sors_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 1, "heather"))
sors_Data <- sors_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 2, "borage"))
sors_Data <- sors_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 4, "multifloral"))

# convert factors back to numbers
sors_Data$Fl.1 <- as.factor(sors_Data$Fl.1)
sors_Data$Fl.1 <- as.numeric(sors_Data$Fl.1)

#######################################################################
#####################           knn.floral       ######################
######################################################################
accuracy_kNN_floral<- c()
misclassifications_kNN_floral <- c()
set.seed(123)
for (i in 1:10){
  # spliting data into training and test
  trainSet1 <- stratified(sors_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- sors_Data %>% setdiff(trainSet1)
  trainSet <- sors_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(testSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
  
  
  sors_knn<- train(trainSet,as.factor(trainCl), 
                   method = "knn",
                   metric= "Accuracy",
                   trControl = ctrl,tuneLength= 20)
  
  predicts_kNN_floral<- predict(sors_knn,testSet)
  confus_kNN_floral<- confusionMatrix(as.factor(predicts_kNN_floral), as.factor(testCl))
  accuracy_kNN_floral<- c(confus_kNN_floral$overall[1],accuracy_kNN_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_kNN_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_kNN_floral))
    
    
    misclassifications_kNN_floral[[key]] <- misclassifications_kNN_floral[[key]] + length(falseNegatives)
  }
}
misclassify <-(misclassifications_kNN_floral/(i*length(predicts_kNN_floral))) *100
x11()
plot(accuracy_kNN_floral, type="l", ylab = "Cummulative accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy","Honey Floral Origin","knn", "over 100 iterations"))

x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100),xlab="class", main=paste("knn",": Misclassified samples per class"), cex.names=0.5)
#######################################################################
#####################           svmPoly.floral       ##################
######################################################################
accuracy_poly_floral<- c()
misclassifications_poly_floral <- c()

set.seed(2008)
for (i in 1:10){
  # spliting data into training and test
  trainSet1 <- stratified(sors_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- sors_Data %>% setdiff(trainSet1)
  trainSet <- sors_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(trainSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
  
  sors_svmPoly<- train(trainSet,as.factor(trainCl),  method = "svmPoly",
                       metric= "Accuracy",
                       trControl = ctrl, tuneLength = 20)
  
  predicts_poly_floral<- predict(sors_svmPoly,testSet)
  confus_poly_floral<- (confusionMatrix(as.factor(predicts_poly_floral), as.factor(testCl)))
  accuracy_poly_floral<- c(confus_poly_floral$overall[1],accuracy_poly_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_poly_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_poly_floral))
    
    misclassifications_poly_floral[[key]] <- misclassifications_poly_floral[[key]] + length(falseNegatives)
  }
}
misclassify <-(misclassifications_poly_floral/(i*length(predicts_poly_floral)))*100

x11()
plot(accuracy_poly_floral, type="l", ylab = "Cummulative accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","Honey Floral Origin","svmPoly", "over 10 iterations"))

x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("svmPoly",": Misclassified samples per class"), cex.names=0.5)
mean(accuracy_poly_floral)
