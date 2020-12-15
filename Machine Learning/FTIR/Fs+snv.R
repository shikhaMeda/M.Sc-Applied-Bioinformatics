#library(Boruta)
library(mlbench)
library(randomForest)
library(caret)
library(dplyr)
library(openxlsx)
library(factoextra)
require(tidyr)
require(tidyverse)
library(splitstackshape)
library(tidyverse)
require("dplyr")
library(randomForest)
library(e1071)
library(kernlab)

# data
DATA <- read.csv("FTIR_1_54.csv", row.names = 1, header = T)                 #read spectral data
category<-read.csv("honey_origin_m.csv", row.names=1, header = T)            #read categorical data

DATA <- DATA[-32,]                                                           #remove sample 32 (experimental error) 
category<-category[-32,]
Data_category<-cbind(DATA,category)

Data_floral.origin<-Data_category[,-c(1868:1869)]                            #remove area and geographical origin
floral.origin<-as.factor(Data_floral.origin$Floral.origin)                   #floral category as factor

Fl.1<-as.factor(Data_floral.origin$Floral.origin)
ftir_spectra<-Data_floral.origin[,-1867]
ftir_Data<-cbind(ftir_spectra,Fl.1)
ftir_Data<-as.data.frame(ftir_Data)

# convert factors to numbers
library(magrittr)
ftir_Data$Fl.1 %<>% factor
ftir_Data$Fl.1 <- as.numeric(ftir_Data$Fl.1)

# mutate bell and ling as one class
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 3, 1))
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 1, "heather"))
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 2, "borage"))
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 4, "multifloral"))

# convert factors back to numbers
ftir_Data$Fl.1 <- as.factor(ftir_Data$Fl.1)
ftir_Data$Fl.1 <- as.numeric(ftir_Data$Fl.1)
str(ftir_Data)

# feature selection
#set.seed(123)
#Boruta(Fl.1 ~ ., data =  ftir_Data, doTrace = 2, maxRuns = 2000)    # Boruta considers all variables unimportant, hence wasn't applied

####Computing feature importance from RF
RF.fs <- train(as.factor(Fl.1) ~ ., method='rf', ftir_Data, tuneGrid=expand.grid(mtry=1:20), importance = T)
#####################################
k<-varImp(RF.fs, scale=F)                      # identifying important variables
k$importance                                                
feature_ID <- c()                                     # saving important features
for(i in 1:nrow(k$importance)){
  if(k$importance[i,1] >= 0.5){
    feature_ID[length(feature_ID)+1] <- i+1
  }
}

ftir_Data <- ftir_Data[,-feature_ID]

# save the .xlsx file if needed, although its already saved
#write.xlsx(ftir_Data, "ftir_Floral_imp.xlsx", sheetName = "Sheet1", col.names = TRUE, row.names = TRUE)

# SNV
ftir_spectra <- ftir_Data[,-1640]
tFTIR_spectra <- t(ftir_spectra)                                            # transpose spectra
FTIR_snv <- scale(tFTIR_spectra, center = T, scale = T)                      # snv on spectra

ftir_spectra <- t(FTIR_snv)                                                  # transpose back into original structure
ftir_Data <- ftir_Data[,-1640]
ftir_Data<-cbind(ftir_spectra,Fl.1)                                         # combine spectra to floral origin category
ftir_Data<-as.data.frame(ftir_Data)                                         # set as data frame matrix
#convert factors to numbers



ftir_Data$Fl.1 <- as.numeric(ftir_Data$Fl.1)                                # specify numric values (4 levels)

# mutate bell and ling as one class
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 3, 1))            # heather as one class
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 1, "heather"))           # assign level number to classes
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 2, "borage"))
ftir_Data <- ftir_Data %>% mutate(Fl.1 = replace(Fl.1, Fl.1 == 4, "multifloral"))

# convert factors back to numbers
ftir_Data$Fl.1 <- as.factor(ftir_Data$Fl.1)                             # now we have three classes as factors instead of 4 factors
ftir_Data$Fl.1 <- as.numeric(ftir_Data$Fl.1)

#######################################################################
#####################           knn.floral       ######################
######################################################################
accuracy_kNN_floral<- c()
misclassifications_kNN_floral <- c()
set.seed(123)
for (i in 1:100){
  # spliting data into training and test
  trainSet1 <- stratified(ftir_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(trainSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
  
  
  ftir_knn<- train(trainSet,as.factor(trainCl), 
                   method = "knn",
                   metric= "Accuracy",
                   trControl = ctrl,tuneLength= 20)
  
  predicts_kNN_floral<- predict(ftir_knn,testSet)
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
     main = paste("Accuracy","Honey floral origin","knn", "over 100 iterations"))

x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100),xlab="class", main=paste("knn",": Misclassified samples per class"), cex.names=0.5)

######################################################################
#####################         rf.floral       ########################
######################################################################
accuracy_rf_floral<- c()
misclassifications_rf_floral <- c()
set.seed(2008)
for (i in 1:100){
  trainSet1 <- stratified(ftir_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(trainSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 5, search="random")
  ftir_rf<- train(trainSet,as.factor(trainCl), 
                  method = "rf",
                  metric= "Accuracy", 
                  trControl = ctrl,
                  tuneLength = 20)
  
  predicts_rf_floral<- predict(ftir_rf,testSet)
  confus_rf_floral<- confusionMatrix(as.factor(predicts_rf_floral), as.factor(testCl))
  accuracy_rf_floral<- c(confus_rf_floral$overall[1],accuracy_rf_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_rf_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_rf_floral))
    misclassifications_rf_floral[[key]] <- misclassifications_rf_floral[[key]] + length(falseNegatives)
  }
}
misclassify <-(misclassifications_rf_floral/(i*length(predicts_rf_floral))) *100

x11()
plot(accuracy_rf_floral, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","Honey Floral Origin","rf", "over 100 iterations"))

imp<-varImp(ftir_rf)
plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","rf"), cex.names=0.5)

x11()
print(plot.vim)

x11()
barplot(misclassify, ylab="misclassifications over 100 iterations",ylim = c(0,100), xlab="class", main=paste("rf",": Misclassified samples per class"), cex.names=0.5)

#######################################################################
#####################           svmPoly       #########################
######################################################################
accuracy_poly_floral<- c()
misclassifications_poly_floral <- c()

set.seed(2008)
for (i in 1:100){
  # spliting data into training and test
  trainSet1 <- stratified(ftir_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(trainSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
  
  ftir_svmPoly<- train(trainSet,as.factor(trainCl),  method = "svmPoly",
                       metric= "Accuracy",
                       trControl = ctrl, tuneLength = 20)
  
  predicsts_poly_floral<- predict(ftir_svmPoly,testSet)
  confus_poly_floral<- (confusionMatrix(as.factor(predicsts_poly_floral), as.factor(testCl)))
  accuracy_poly_floral<- c(confus_poly_floral$overall[1],accuracy_poly_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_poly_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicsts_poly_floral))
  misclassifications_poly_floral[[key]] <- misclassification  zMs_poly_floral[[key]] + length(falseNegatives)
  }
}

misclassify <-(misclassifications_poly_floral/(i*length(predicsts_poly_floral)))*100

x11()
plot(accuracy_poly_floral, type="l", ylab = "Cummulative accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","HoneyC","svmPoly", "over 100 iterations"))


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("svmPoly",": Misclassified samples per class"), cex.names=0.5)

svm.acc.m<- mean(accuracy_poly_floral)
SD <- sd(accuracy_poly_floral)                    # standard deviation
CI95<- gmodels::ci(accuracy_poly_floral,confidence =0.95)  

#plot accuracy with 95% confidence interval
x11()
plot(accuracy_poly_floral,type = "l",ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste0(" Accuracy Honey Floral Origin SVM over 100 iterations :\n mean accuracy = " ,
                   round(svm.acc.m,digits = 2),"; SD " ,
                   round(SD,digits = 2),"; 95% Confidence Interval:" ,
                   round(CI95[2],digits = 2),"-",
                   round(CI95[3],digits = 2), "\n"))

