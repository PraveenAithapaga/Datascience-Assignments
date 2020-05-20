library(factoextra)
library(cluster)
install.packages("fpc")
installed.packages("NbClust")
library(fpc)
library(NbClust)

mydata<-read.csv(file.choose(),header = T)
View(mydata)
View(mydata[-1])
data <- mydata[,-1]
attach(data)
cor(data)

pcaObj<-princomp(data, cor = TRUE, scores = TRUE, covmat = NULL)

str(pcaObj)
summary(pcaObj)
str(pcaObj)
loadings(pcaObj)
plot(pcaObj)
biplot(pcaObj)

plot(cumsum(pcaObj$sdev*pcaObj$sdev)*100/(sum(pcaObj$sdev*pcaObj$sdev)),type="b")
pcaObj$scores[,1:3]


mydata<-cbind(mydata,pcaObj$scores[,1:3])
View(mydata)

# Hierarchial Clustering
# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,8:10]

# Normalizing the data
norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance

fit1<-hclust(dist1,method="complete") # method here is complete linkage

plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,7) # Cutting the dendrogram for 7 clusters

membership_1<-as.matrix(groups) # cluster numbering

View(membership_1)

final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)
View(aggregate(final1[,-c(2,16:18)],by=list(membership_1),FUN=mean))


# K-Means Clustering :
library(plyr)
mydata <- read.csv(file.choose())
str(mydata)
View(mydata)

normalized_data<-scale(mydata[,15:17])

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))     # Determine number of clusters by scree-plot
for (i in 1:7) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)
plot(1:7, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")   # Look for an "elbow" in the scree plot #
title(sub = "K-Means Clustering Scree-Plot")

fit <- eclust(normalized_data, "kmeans", k = 7, nstart = 25, graph = FALSE) # 7 cluster solution
fviz_cluster(fit, geom = "point", frame.type = "norm")

final2<- data.frame(fit$cluster,mydata) # append cluster membership
View(final2)
aggregate(mydata[,2:17], by=list(fit$cluster), FUN=mean)

table(fit$cluster)




rect.hclust(fit1, k=7, border="red")

