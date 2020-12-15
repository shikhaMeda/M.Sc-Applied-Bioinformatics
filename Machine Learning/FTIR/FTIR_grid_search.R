########################################################################################################################

                                         # FTIR analysis using tune grid

##########################################################################################################################

library(caret)
library(factoextra)
library(tidyr)
library(tidyverse)
library(splitstackshape)
library(tidyverse)
library(ggstatsplot)
library(dplyr)
library(randomForest)
#library("mlr")            # training pls da
library(mlr)
library(ggplot2)
#install.packages("John Fox's car ")

# Read dataset
DATA <- read.csv("FTIR_1_54.csv", row.names = 1, header = T)      

# Remove sample 32 from dataset
DATA <- DATA[-32,]
category<-read.csv("honey_origin_m.csv", row.names=1, header = T)
category<-category[-32,]
Data_category<-cbind(DATA,category)   


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ##                   Floral Origin           ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
# Prepare dataset for floral origin
Data_floral.origin<-Data_category[,-c(1868:1869)]          # removing area and geographical origin
floral.origin<-as.factor(Data_floral.origin$Floral.origin)            # select floral origin

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

##############################  kNN floral origin #############################################
accuracy_knn_floral<- c()
misclassifications_knn_floral <- c()
set.seed(2008)
for (i in 100){
  # spliting data into training and test
  trainSet1 <- stratified(ftir_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(testSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
 ctrl <- trainControl(method = "repeatedcv", repeats = 5)
 ftir_knn<- train(trainSet,as.factor(trainCl), 
                   method = "knn",
                   metric= "Accuracy",
                   trControl = ctrl,tuneGrid = expand.grid(k = c(1:20)))
 print(ftir_knn$results)  
 
 predicts_knn_floral<- predict(ftir_knn,testSet)
 confus_knn_floral<- confusionMatrix(as.factor(predicts_knn_floral), as.factor(testCl))
 accuracy_knn_floral<- sum(confus_knn_floral$overall[1],accuracy_knn_floral)
 keys <- unique(testCl)
 for(key in keys) {
   if (i == 1) { misclassifications_knn_floral[[key]] <- 0 }
   falseNegatives <- which((testCl == key) & (testCl != predicts_knn_floral))
   misclassifications_knn_floral[[key]] <- misclassifications_knn_floral[[key]] + length(falseNegatives)
 }
}

misclassifications_knn_floral <-(misclassifications_knn_floral/(i*length(predicts_knn_floral))) *100
x11()
plot(accuracy_knn_floral, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","honey Floral Origin","knn", "over 100 iterations"))


x11()
barplot(misclassifications_knn_floral, ylab="misclassifications over 100 iterations", ylim = c(0,100),xlab="class", main=paste("knn",": Misclassified samples per class"), cex.names=0.5)

######################################################################
#####################         rf       ###############################
######################################################################
accuracy_rf_floral<- c()
misclassifications_rf_floral <- c()
set.seed(2008)
for (i in 1:2){
  trainSet1 <- stratified(ftir_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(testSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]

  ctrl <- trainControl(method = "repeatedcv", number=10, repeats = 5)
  
   ftir_rf<- train(trainSet,as.factor(trainCl), 
                  method = "rf",
                  metric= "Accuracy", 
                  trControl = ctrl,
                  tuneGrid = expand.grid(.mtry=c(1:20, ntree = 500:1000)))
  print(ftir_rf$results)
  predicts_rf_floral<- predict(ftir_rf,testSet)
  confus_rf_floral<- confusionMatrix(as.factor(predicts_rf_floral), as.factor(testCl))
  accuracy_rf_floral<- c(confus_rf_floral$overall[1],accuracy_rf_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_rf[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_rf))
    #percentMisclassification <- (length(falseNegatives)/length(predicts_rf))*100
    misclassifications_rf[[key]] <- misclassifications_rf[[key]] + length(falseNegatives)
    
  }
}
print(ftir_rf)
plot(ftir_rf)
misclassify <-(misclassifications_rf_floral/(i*length(predicts_rf_floral))) *100
x11()
plot(accuracy_rf_floral, type="l", ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","honey Floral Origin","rf", "over 100 iterations"))

#imp<-varImp(ftir_rf)
#plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","rf"), cex.names=0.5)

#x11()
#print(plot.vim)


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations",ylim = c(0,100), xlab="class", main=paste("rf",": Misclassified samples per class"), cex.names=0.5)


#######################################################################
#####################           svmPoly       #########################
######################################################################
accuracy_poly_floral<- c()
misclassifications_poly_floral <- c()

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
  
  gridds <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5), degree=c(1:3), scale=c(1:3))
  
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, returnData = T,allowParallel = T) # keep param optimization
                       

  
  ftir_svmPoly<- train(trainSet,as.factor(trainCl),  method = "svmPoly",  # train over cost, degree and scale
                       metric= "Accuracy", trControl = ctrl,
     tuneGrid = gridds)
  # preProcess = c("center", "scale")
  predicts_poly_floral<- predict(ftir_svmPoly,testSet)
  confus_poly_floral<- (confusionMatrix(as.factor(predicts_poly_floral), as.factor(testCl)))
  accuracy_poly_floral<- c(confus_poly_floral$overall[1],accuracy_poly_floral)
  keys <- unique(testCl)
  for(key in keys) {
    if (i == 1) { misclassifications_poly_floral[[key]] <- 0 }
    falseNegatives <- which((testCl == key) & (testCl != predicts_poly_floral))
    #percentMisclassification <- (length(falseNegatives)/length(predicsts_poly))*100
    misclassifications_poly_floral[[key]] <- misclassifications_poly_floral[[key]] + length(falseNegatives)
  }
}
misclassify <-(misclassifications_poly_floral/(i*length(predicts_poly_floral)))*100
x11()
plot(accuracy_poly_floral, type="l", ylab = "Cummulative accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","Honey Floral Origin","svmPoly", "over 100 iterations"))

#imp<-varImp(ftir_svmPoly)
#plot.vim <- plot(imp,main = paste("Variable importance of","HOnc","svmPoly"), cex.names=0.5)

#x11()
#print(plot.vim)


x11()
barplot(misclassifications_poly_floral, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("svmPoly",": Misclassified samples per class"), cex.names=0.5)
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
  
  #arrange resampling with replacement for imbalanced classes
  ctrl <- trainControl(method = "repeatedcv", repeats=5, number = 10)


search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0:5,
 adjust = seq(0, 5, by = 1)
)

ftir_nb<- train(trainSet,as.factor(trainCl), "nb",
                  metric= "Accuracy",
                  trControl = ctrl,
                  tuneGrid =  search_grid)
                  
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
plot(accuracy_nb_floral, type="l", ylab = "Cummulative accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste("Accuracy of","Honey Floral Origin","naiveBayes", "over 100 iterations"))


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations", ylim = c(0,100), xlab="class", main=paste("naiveBayes",": Misclassified samples per class"), cex.names=0.5)

###############################################################################
#####################   pls-da ###############################################
##############################################################################
accuracy_pls_floral<- c()
misclassifications_pls_floral <- c()
#confus<-c()
set.seed(2008)
for (i in 1){
  trainSet1 <- stratified(ftir_Data, "Fl.1", .7, select = list(Fl.1 = c("1", "2", "3")))
  
  testSet <- ftir_Data %>% setdiff(trainSet1)
  trainSet <- ftir_Data %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(testSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
 
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random")
  grid_plsda <- c(5:10,  seq(20, 100, 10))
  ftir_pls<- train(trainSet,as.factor(trainCl), 
                   
                    metric= "Accuracy", 
                   Method = "plsda",
                   trainControl = ctrl,
                   tuneGrid = grid_plsda)
                    
  
  
  predicts_pls_floral<- predict(ftir_pls,testSet)
  confus_pls<- confusionMatrix(as.factor(predicts_pls_floral), as.factor(testCl))
  accuracy_pls_floral<- c(confus_pls$overall[1],accuracy_pls_floral)
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
