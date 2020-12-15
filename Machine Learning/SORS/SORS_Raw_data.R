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
library(magrittr)
#Prepare dataset for analysis
DATA <- read.csv("SORS_av.csv", row.names = 1, header = T)   
category<-read.csv("honey_origin_m.csv", row.names=1, header = T)

category<-category[-32,]        # - 32 sample number
Data_category<-cbind(DATA,category)

Data_floral.origin<-Data_category[,-c(1026:1027)]                # exclude geographical origin and area
floral.origin<-as.factor(Data_floral.origin$Floral.origin)

Fl.1<-as.factor(Data_floral.origin$Floral.origin)
sors_spectra<-Data_floral.origin[,-1025]      # only sepctra
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
for (i in 1:100){
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

knn.acc.m <- mean(accuracy_kNN_floral)           # mean accuracy
SD <- sd(accuracy_kNN_floral)                    # standard deviation
CI95<- gmodels::ci(accuracy_kNN_floral,confidence =0.95)  

#plot accuracy with 95% confidence interval
x11()
plot(accuracy_kNN_floral,type = "l",ylab = "cummulative accuracy", xlab = "no. iterations",ylim = c(0,1),
     main = paste0(" Accuracy Honey Floral Origin kNN over 100 iterations :\n mean accuracy = " ,
                   round(knn.acc.m,digits = 2),"; SD " ,
                   round(SD,digits = 2),"; 95% Confidence Interval:" ,
                   round(CI95[2],digits = 2),"-",
                   round(CI95[3],digits = 2), "\n"))
######################################################################
#####################         rf.floral      ###############################
######################################################################
accuracy_rf_floral<- c()
misclassifications_rf_floral <- c()
#confus<-c()
set.seed(2008)
for (i in 1:100){
  trainSet1 <- stratified(sors_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- sors_Data %>% setdiff(trainSet1)
  trainSet <- sors_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(trainSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 5, search="random")
  
  
  sors_rf<- train(trainSet,as.factor(trainCl), 
                  method = "rf",
                  metric= "Accuracy", 
                  trControl = ctrl,
                  tuneLength = 20)
  
  predicts_rf_floral<- predict(sors_rf,testSet)
  confus_rf_floral<- confusionMatrix(as.factor(predicts_rf_floral), as.factor(testCl))
  accuracy_rf_floral<- c(confus_rf_floral$overall[1],accuracy_rf_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_rf_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_rf_floral))
    #percentMisclassification <- (length(falseNegatives)/length(predicts_rf_floral))*100
    misclassifications_rf_floral[[key]] <- misclassifications_rf_floral[[key]] + length(falseNegatives)
    
  }
}

misclassify <-(misclassifications_rf_floral/(i*length(predicts_rf_floral))) *100

x11()
plot(accuracy_rf_floral, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","Honey Floral Origin","rf", "over 100 iterations"))

imp<-varImp(ftir_rf)
plot.vim <- plot(imp,main = paste("Variable importance of","Honey Floral Origin","rf"), cex.names=0.5)

x11()
print(plot.vim)


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations",ylim = c(0,100), xlab="class", main=paste("rf",": Misclassified samples per class"), cex.names=0.5)

mean(accuracy_rf_floral)
#######################################################################
#####################           svmPoly.floral       ##################
######################################################################
accuracy_poly_floral<- c()
misclassifications_poly_floral <- c()

set.seed(2008)
for (i in 1:100){
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
     main = paste("Accuracy of","Honey Floral Origin","svmPoly", "over 100 iterations"))

x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("svmPoly",": Misclassified samples per class"), cex.names=0.5)
mean(accuracy_poly_floral)

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