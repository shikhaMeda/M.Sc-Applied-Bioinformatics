##############################################
# Photonics pca
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
Phot_spectra <- Phot_all[,-c(1793:1795)]                                      # only spectra

# specify factor category
floral.origin<-Phot_all[,1793]                                               
cl.1 <-as.factor(floral.origin)
cl.1<- as.factor(cl.1)

#pca
pca.phot <- prcomp(Phot_spectra,retx=T, center=T, scale=T)
x11()
fviz_eig(pca.phot)                                                          # Plot the eigenvalues/variances against the number of dimensions

# plot pca 
x11()
fviz_pca_ind(pca.phot,
             axes = c(1,2),
             geom.ind = c("point", "text"),
             col.ind = floral.origin,
             palette = c("#FC4E07", "#41AF9A", "#DA85A2", "#95A563"),
             addEllipses = TRUE,
             #ellipse.type="confidence",
             legend.title = "floral origin",
             title = "PCA Honey botanical origin - Fluorescence",
             repel = TRUE     # Avoid text overlapping
)

# pca dataset 
Phot.PC <-pca.phot$x[,1:8] 
Phot.PC <- cbind(Phot.PC,cl.1)
Phot.PC <- as.data.frame(Phot.PC)

# mutate bell and ling as one class
Phot.PC <- Phot.PC %>% mutate(cl.1 = replace(cl.1, cl.1 == 3, 1))
Phot.PC <- Phot.PC %>% mutate(cl.1 = replace(cl.1, cl.1 == 1, "heather"))
Phot.PC <- Phot.PC %>% mutate(cl.1 = replace(cl.1, cl.1 == 2, "borage"))
Phot.PC <- Phot.PC %>% mutate(cl.1 = replace(cl.1, cl.1 == 4, "multifloral"))

# convert factors back to numbers
Phot.PC$cl.1 <- as.factor(Phot.PC$cl.1)
Phot.PC$cl.1 <- as.numeric(Phot.PC$cl.1)

#######################################################################
#####################           knn.floral       ######################
######################################################################
accuracy_kNN_floral<- c()
misclassifications_kNN_floral <- c()
set.seed(123)
for (i in 1:100){
  # spliting data into training and test
  trainSet1 <- stratified(Phot.PC, "cl.1", .7, select = list(cl.1 = c("1", "2", "3")))
  
  testSet <- Phot.PC %>% setdiff(trainSet1)
  trainSet <- Phot.PC %>% setdiff(testSet)
  
  
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

#####################################################################
#####################         rf.floral      #########################
######################################################################
accuracy_rf_floral<- c()
misclassifications_rf_floral <- c()

set.seed(2008)
for (i in 1:100){
  trainSet1 <- stratified(Phot.PC, "cl.1", .7, select = list(cl.1 = c("1", "2", "3")))
  
  testSet <- Phot.PC %>% setdiff(trainSet1)
  trainSet <- Phot.PC %>% setdiff(testSet)
  
  
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
  trainSet1 <- stratified(Phot.PC, "cl.1", .7, select = list(cl.1 = c("1", "2", "3")))
  
  testSet <- Phot.PC %>% setdiff(trainSet1)
  trainSet <- Phot.PC %>% setdiff(testSet)
  
  
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
