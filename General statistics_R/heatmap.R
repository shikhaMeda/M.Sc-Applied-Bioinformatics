library(openxlsx)
library(readxl)
library(tidyverse)
library(ggforce)   #add circles
library(limma)
library(readxl)
library(gplots)
library(factoextra)
library(stats)
library(graphics)
library(utils)
Data <- read_excel("Orchid_paper/Metabolites/All_groups_orchid_metabolites.xlsx", 
                   sheet = "common_metab")
#Data1 <- t(Data)
#my.names <- Data1[1,]
#colnames(Data1) <- my.names
#Data1 <- Data1[-1, ]
#Data1 <- data.frame(Data1)
#attach(Data)
#W <- (Wild_Control >=500)
#M <- (Mutant_Control >=500)
#t <- (MF_Treatment >=500)
#All <- cbind(W, M, t) 
#a <- vennCounts(All)
#vennDiagram(a)

#heatmap


y <- data.frame(Data)
#heatmap.2(log_y, main = "Metabolites expressed", trace = "none", margins = c(10,18))
row.names(y) <- y$Metabolites
y <- data.matrix(y)
y <- y[,-1]
#plot(y)
#log_y <- log10(y+1)
#plot(log_y)

#clustering
fviz_nbclust(y, kmeans, method = "wss")
fviz_nbclust(y, kmeans, method = "silhouette")
final <- kmeans(y, 2, nstart = 1)
fviz_cluster(final, data = y, labelsize = 13)

hr <- hclust(as.dist(1-cor(t(y), method = "pearson")), method = "complete")
hc <- hclust(as.dist(1-cor(y,method = "spearman")), method = "complete")
mycl <- cutree(hr, h=max(hr$height)/1.5); mycolhc <- rainbow(length(unique(as.vector(mycl))), start = 0.1, end = 0.9); mycolhc[as.vector(mycl)]
cluster <- as.matrix(mycl)
output <- cbind(y,cluster)
mycol <- colorpanel(30, "Red", "white", "Dark Blue")
heatmap.2(y,  main = "Metabolites expression", Rowv = as.dendrogram(hr),Colv = as.dendrogram(hc), 
          col = mycol, density.info = "none", trace = "none", dendrogram = "both", 
          key.xlab = "Low to high expression", key.ylab = "NA", scale = "row","column",
          c(labRow = T,labCol = T), margins = c(10,20))
library(ggdendro)
install.packages("ggdendro")
library(ggplot2)

