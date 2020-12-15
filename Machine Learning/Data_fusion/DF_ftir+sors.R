#############################################
      # Data Fusion for ftir and sors
##############################################
library(openxlsx)               
library(factoextra)     #fviz_eig  
library(dplyr)  #%>% mutate
library(splitstackshape) #stratify 
library(caret)
library(magrittr)
# Finding principle components to use as dataset
Phot_all <- read.xlsx("Photonics.xlsx", sheet = 1, rowNames = T)              # load spectral data
honey.origin <- read.csv("honey_origin_m.csv", row.names = 1, header = T)     # load categorical data                                        
honey.origin <- honey.origin[-32,]                                            # remove sample 32 (fault in sample due to experimental reasons)                        
Phot_spectra <- Phot_all[,-c(1793:1795)] 

#Standardizing the spectra data
# Using SNV to standardize and Normalize the raw spectra data
Phot_spectra <- t(Phot_spectra)                                              # transpose spectra
phot_snv <- scale(Phot_spectra, center = T, scale = T)  
Phot_spectra <- t(phot_snv)                                                  # resultant is the transformed spectra
Phot_spectra <- as.matrix(Phot_spectra)

floral.origin<-Phot_all[,1793]
cl.1 <-as.factor(floral.origin)
cl.1<- as.factor(cl.1)

#PCA and plots
pca.phot <- prcomp(Phot_spectra,retx=T, center=T, scale=T)
x11()
fviz_eig(pca.phot) 

Phot.PC <-pca.phot$x[,1:10]
####################### FTIR ############################
FTIR_spec <- read.csv("FTIR_1_54.csv", row.names = 1, header = T)
FTIR_spec <- FTIR_spec[-32,]
floral.origin1 <- read.csv("honey_origin_m.csv", row.names = 1, header = T)
floral.origin1 <- floral.origin1[-32,]
row.names(FTIR_spec) <- gsub("Sample", "S", row.names(FTIR_spec))        # change row names to make it similar to other two data types and for better visualisation

FTIR.All <- cbind(FTIR_spec,floral.origin1)                              # spectra along with floral origin 
FTIR_spec <- FTIR.All[,-c(1867:1869)]                                    # spectra only 

# SNV
FTIR_spec <- t(FTIR_spec)                                          
FTIR_snv <- scale(FTIR_spec, center = T, scale = T)  
FTIR_spec <- t(FTIR_snv)                                               

FTIR_spec <- as.matrix(FTIR_spec)
floral.origin1<-FTIR.All[,1867]
cl.2 <-as.factor(floral.origin1)
cl.2<- as.factor(cl.2)

#PCA and plots
pca.ftir <- prcomp(FTIR_spec,retx=T, center=T, scale=T)
x11()
fviz_eig(pca.ftir)

ftir.PC <-pca.ftir$x[,1:8] 

################## SORS ###########################

SORS_spec <- read.csv("SORS_av.csv", row.names = 1, header = T)
floral.origin2 <- read.csv("honey_origin_m.csv", row.names = 1, header = T)
floral.origin2 <- floral.origin2[-32,]                             # subtract sample 32 (experimental error)
floral.origin2 <- floral.origin2[ -47,]                            # subtract sample 48 (experimental error)
row.names(SORS_spec) <- gsub("Sample", "S", row.names(SORS_spec))  # change row names to make it similar to other two data types

SORS.All <- cbind(SORS_spec,floral.origin2)
SORS_spec <- SORS.All[,-c(1025:1027)]

# SNV
SORS_spec <- t(SORS_spec)                                              
SORS_snv <- scale(SORS_spec, center = T, scale = T)  
SORS_spec <- t(SORS_snv)                                              

SORS_spec <- as.matrix(SORS_spec)
floral.origin2<-SORS.All[,1025]
cl.3 <-as.factor(floral.origin2)
cl.3<- as.factor(cl.3)


#PCA and plots
pca.sors <- prcomp(SORS_spec,retx=T, center=T, scale=T)
x11()
fviz_eig(pca.sors)

Sors.PC <-pca.sors$x[,1:6]


Sors.PC <- as.data.frame(Sors.PC)
ftir.PC <- ftir.PC[-47,]
ftir.PC <- as.data.frame(ftir.PC)
Phot.PC <- Phot.PC[-47,]
Phot.PC <- as.data.frame(Phot.PC)


colnames(ftir.PC) <- gsub("1", "11", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("2", "12", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("3", "13", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("4", "14", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("5", "15", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("6", "16", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("7", "17", colnames(ftir.PC))
colnames(ftir.PC) <- gsub("8", "18", colnames(ftir.PC))

colnames(Sors.PC) <- gsub("1", "19", colnames(Sors.PC))
colnames(Sors.PC) <- gsub("2", "20", colnames(Sors.PC))
colnames(Sors.PC) <- gsub("3", "21", colnames(Sors.PC))
colnames(Sors.PC) <- gsub("4", "22", colnames(Sors.PC))
colnames(Sors.PC) <- gsub("5", "23", colnames(Sors.PC))
colnames(Sors.PC) <- gsub("6", "24", colnames(Sors.PC))

All.pc <- cbind(ftir.PC, Sors.PC)               # merge PC's FROM the three dataset
Allpc.floral <- cbind(All.pc,floral.origin2)             # combine predictors with categories

# convert factors to numbers
Allpc.floral$floral.origin2 %<>% factor
Allpc.floral$floral.origin2 <- as.numeric(Allpc.floral$floral.origin2)

# mutate bell and ling as one class
Allpc.floral <- Allpc.floral %>% mutate(floral.origin2 = replace(floral.origin2, floral.origin2 == 3, 1))
Allpc.floral <- Allpc.floral %>% mutate(floral.origin2 = replace(floral.origin2, floral.origin2 == 1, "heather"))
Allpc.floral <- Allpc.floral %>% mutate(floral.origin2 = replace(floral.origin2, floral.origin2 == 2, "borage"))
Allpc.floral <- Allpc.floral %>% mutate(floral.origin2 = replace(floral.origin2, floral.origin2 == 4, "multifloral"))

# convert factors back to numbers
Allpc.floral$floral.origin2 <- as.factor(Allpc.floral$floral.origin2)
Allpc.floral$floral.origin2 <- as.numeric(Allpc.floral$floral.origin2)

#######################################################################
#####################           knn.floral       ######################
######################################################################
accuracy_kNN_floral<- c()
misclassifications_kNN_floral <- c()
set.seed(123)
for (i in 1:100){
  # spliting data into training and test
  trainSet1 <- stratified(Allpc.floral, "floral.origin2", .7, select = list(floral.origin2 = c("1", "2", "3")))
  
  testSet <- Allpc.floral %>% setdiff(trainSet1)
  trainSet <- Allpc.floral %>% setdiff(testSet)
  
  
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
                   trControl = ctrl,tuneLength= 10)
  
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
#####################         rf.floral      #########################
######################################################################
accuracy_rf_floral<- c()
misclassifications_rf_floral <- c()

set.seed(2008)
for (i in 1:100){
  trainSet1 <- stratified(Allpc.floral, "floral.origin2", .7, select = list(floral.origin2 = c("1", "2", "3")))
  
  testSet <- Allpc.floral %>% setdiff(trainSet1)
  trainSet <- Allpc.floral %>% setdiff(testSet)
  
  
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
    #percentMisclassification <- (length(falseNegatives)/length(predicts_rf_floral))*100
    misclassifications_rf_floral[[key]] <- misclassifications_rf_floral[[key]] + length(falseNegatives)
    
  }
}

misclassify <-(misclassifications_rf_floral/(i*length(predicts_rf_floral))) *100

imp<-varImp(ftir_rf)
plot.vim <- plot(imp,main = paste("Variable importance of","Honey Floral Origin","rf"), cex.names=0.5)

x11()
print(plot.vim)


x11()
barplot(misclassify, ylab="misclassifications over 100 iterations",ylim = c(0,100), xlab="class", main=paste("rf",": Misclassified samples per class"), cex.names=0.5)

rf.acc.m<- mean(accuracy_rf_floral)
SD <- sd(accuracy_rf_floral)                    # standard deviation
CI95<- gmodels::ci(accuracy_rf_floral,confidence =0.95)  

#plot accuracy with 95% confidence interval
x11()
plot(accuracy_rf_floral,type = "l",ylab = "Cummulative Accuracy", xlab = "Iterations",ylim = c(0,1),
     main = paste0(" Accuracy Honey Floral Origin RF over 100 iterations :\n mean accuracy = " ,
                   round(rf.acc.m,digits = 2),"; SD " ,
                   round(SD,digits = 2),"; 95% Confidence Interval:" ,
                   round(CI95[2],digits = 2),"-",
                   round(CI95[3],digits = 2), "\n"))

#######################################################################
#####################           svmPoly.floral       ##################
######################################################################
accuracy_poly_floral<- c()
misclassifications_poly_floral <- c()

set.seed(2008)
for (i in 1:100){
  # spliting data into training and test
  trainSet1 <- stratified(Allpc.floral, "floral.origin2", .7, select = list(floral.origin2 = c("1", "2", "3")))
  
  testSet <- Allpc.floral %>% setdiff(trainSet1)
  trainSet <- Allpc.floral %>% setdiff(testSet)
  
  
  trainCl <- trainSet[,ncol(trainSet)]
  trainCl <- as.factor(trainCl)
  testCl <- testSet[,ncol(trainSet)]
  testCl <- as.factor(testCl)
  
  trainSet <- trainSet[,1:(ncol(trainSet)-1)]
  testSet <- testSet[,1:(ncol(testSet)-1)]
  
  
  ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, search = "random", sampling = "up")
  
  
  ftir_svmPoly<- train(trainSet,as.factor(trainCl),  method = "svmPoly",
                       metric= "Accuracy",
                       trControl = ctrl, tuneLength = 20)
  
  predicts_poly_floral<- predict(ftir_svmPoly,testSet)
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

