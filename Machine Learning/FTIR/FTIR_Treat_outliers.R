require(caret)
library(factoextra)
require(tidyr)
require(tidyverse)
library(splitstackshape)
library(tidyverse)
require("dplyr")
library(randomForest)
library(e1071)
library(kernlab)
library(mixOmics)
library(mlr)

#Prepare dataset for analysis
DATA <- read.csv("FTIR_1_54.csv", row.names = 1, header = T)                 #read spectral data
category<-read.csv("honey_origin_m.csv", row.names=1, header = T)            #read categorical data

DATA <- DATA[-32,]                                                           #remove sample 32 (experimental error) 
category<-category[-32,]
Data_category<-cbind(DATA,category)

Data_floral.origin<-Data_category[,-c(1868:1869)]                            #remove area and geographical origin
floral.origin<-as.factor(Data_floral.origin$Floral.origin)                   #floral category as factor



# Remove outliers from a column
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
# Removes all outliers from a data set
remove_all_outliers1 <- function(df){
  # We only want the numeric columns
  df[,sapply(df, is.numeric)] <- lapply(df[,sapply(df, is.numeric)], remove_outliers)
  df
}

devoid_outliers <- remove_all_outliers1(Data_floral.origin)



#Remove sample 10 and 11
#devoid_outliers <- devoid_outliers[-c(10:11),]

#replace na with mean
for(i in 1:ncol(devoid_outliers)){
  devoid_outliers[is.na(devoid_outliers[,i]), i] <- mean(devoid_outliers[,i], na.rm = TRUE)
}


Fl.1<-as.factor(devoid_outliers$Floral.origin)
ftir_spectra<-devoid_outliers[,-1867]
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
#x11()
#boxplot <- barplot(table(ftir_Data$Fl.1),
#col = rainbow(3), main = 'Class Distribution')                    


#######################################################################
#####################           knn.floral       ############################
######################################################################
accuracy_knn_floral<- c()
misclassifications_knn_floral <- c()
set.seed(123)
for (i in 1:10){
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
  

  predicts_knn_floral<- predict(ftir_knn,testSet)
  confus_knn_floral<- confusionMatrix(as.factor(predicts_knn_floral), as.factor(testCl))
  accuracy_knn_floral<- c(confus_knn_floral$overall[1],accuracy_knn_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts))
    
    # totalSample <-  which((testCl == key) & (testCl == predicts))
    #percentMisclassification <- (length(falseNegatives)/length(predicts))*100
    misclassifications[[key]] <- misclassifications[[key]] + length(falseNegatives)
  }
}


misclassify <-(misclassifications/(i*length(predicts))) *100
x11()
plot(accuracy, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","HoneyC","knn", "over 100 iterations"))

#imp<-varImp(ftir_svmPoly)
#plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","knn"), cex.names=0.5)

#x11()
#print(plot.vim)


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100),xlab="class", main=paste("knn",": Misclassified samples per class"), cex.names=0.5)


######################################################################
#####################         rf.floral       ########################
######################################################################
accuracy_rf_floral<- c()
misclassifications_rf_floral <- c()
#confus<-c()
set.seed(2008)
for (i in 1:2){
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
    if (i == 1) { misclassifications[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts))
    #percentMisclassification <- (length(falseNegatives)/length(predicts))*100
    misclassifications[[key]] <- misclassifications[[key]] + length(falseNegatives)
    
  }
}
print(ftir_rf)
plot(ftir_rf)
misclassify <-(misclassifications/(i*length(predicts))) *100
x11()
plot(accuracy, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","HoneyC","rf", "over 100 iterations"))

imp<-varImp(ftir_rf)
plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","rf"), cex.names=0.5)

x11()
print(plot.vim)


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations",ylim = c(0,100), xlab="class", main=paste("rf",": Misclassified samples per class"), cex.names=0.5)

mean(accuracy)


#######################################################################
#####################           svmPoly       #########################
######################################################################
accuracy_svm_floral<- c()
misclassifications_svm_floral <- c()

set.seed(2008)
for (i in 1:2){
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
  
  #arrange resampling with replacement for imbalanced classes
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
 
  
  
  ftir_svmPoly<- train(trainSet,as.factor(trainCl),  method = "svmPoly",
                       metric= "Accuracy",
                       trControl = ctrl, tuneLength = 20)
  
  predicts_svm_floral<- predict(ftir_svmPoly,testSet)
  confus_svm_floral<- (confusionMatrix(as.factor(predicts_svm_floral), as.factor(testCl)))
  accuracy_svm_floral<- c(confus_svm_floral$overall[1],accuracy_svm_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_svm_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts))
  
    misclassifications_svm_florals[[key]] <- misclassifications_svm_floral[[key]] + length(falseNegatives)
  }
}
misclassify <-(misclassifications_svm_floral/(i*length(predicts_svm_floral)))*100
x11()
plot(accuracy, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","HoneyC","svmPoly", "over 100 iterations"))

#imp<-varImp(ftir_svmPoly)
#plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","svmPoly"), cex.names=0.5)

#x11()
#print(plot.vim)


x11()
barplot(misclassifications, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("svmPoly",": Misclassified samples per class"), cex.names=0.5)
mean(accuracy)

#######################################################################
#####################          naive bayes      ############################
######################################################################
accuracy_nb_floral<- c()
misclassifications_nb_floral <- c()
set.seed(123)
for (i in 1:2){
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
 
  
  
  ftir_nb<- train(trainSet,as.factor(trainCl), 
                  method = "naive_bayes",
                  metric= "Accuracy",
                  #tuneGrid = search_grid, 
                  trControl = ctrl,
                  luneLength = 20)
  predicts_nb_floral<- predict(ftir_nb,testSet)
  confus_nb_floral<- confusionMatrix(as.factor(predicts_nb_floral), as.factor(testCl))
  accuracy_nb_floral<- c(confus_nb_floral$overall[1],accuracy_nb_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_nb_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_nb_floral))
    misclassifications_nb_floral[[key]] <- misclassifications_nb_floral[[key]] + length(falseNegatives)
  }
}
misclassify <-(misclassifications_nb_floral/(i*length(predicts_nb_floral))) *100
x11()
plot(accuracy, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","HoneyC","naiveBayes", "over 100 iterations"))

imp<-varImp(ftir_nb)
plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","naiveBayes"), cex.names=0.5)

x11()
print(plot.vim)


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("naiveBayes",": Misclassified samples per class"), cex.names=0.5)
###############################################################################
#####################   pls-da ###############################################
##############################################################################
accuracy_pls_floral<- c()
misclassifications_pls_floral <- c()
#confus<-c()
set.seed(2008)
for (i in 1:100){
  trainSet1 <- stratified(ftir_Data, "sensory", .7, select = list(sensory = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(testSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  #arrange resampling with replacement for imbalanced classes
  ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 5, search="random")
  
  
  
  #tunegrid <- expand.grid(.mtry=c(1:15))
  
  ftir_pls<- train(trainSet,as.factor(trainCl), 
                    method = "plsda",
                    metric= "Accuracy", 
                    trControl = ctrl,
                    tuneLength = 20)
  
  
  predicts<- predict(ENOSE_pls,testSet)
  confus_pls<- confusionMatrix(as.factor(predicts), as.factor(testCl))
  accuracy_pls<- c(confus_pls$overall[1],accuracy_pls)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_pls[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts))
    #percentMisclassification <- (length(falseNegatives)/length(predicts))*100
    misclassifications_pls[[key]] <- misclassifications_pls[[key]] + length(falseNegatives)
    
  }
}

misclassifications_pls <-( misclassifications_pls/(i*length(predicts))) *100
x11()
plot(accuracy_pls, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","ENOSE classification","pls", "over 100 iterations"))

x11()
barplot(misclassifications_pls, ylab="misclassifications over 100 iterations",ylim = c(0,100), xlab="class", main=paste("pls",": Misclassified samples per class"), cex.names=0.5)

mean(accuracy_pls)



