mydata <- read.csv(file.choose(),header = T)
View(mydata)

str(mydata)

normalized_data<-scale(mydata[,2:12])
View(normalized_data)

wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))

for (i in 2:12) wss[i] = sum(kmeans(normalized_data, centers=i)$withinss)

plot(1:12, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

title(sub = "K-Means Clustering Scree-Plot")

fit <- kmeans(normalized_data, 3)

final2<- data.frame(mydata, fit$cluster)

aggregate(mydata[,2:12], by=list(fit$cluster), FUN=mean)
table(fit$cluster)


#hirarerical



d <- dist(normalized_data, method = "euclidean") 
d
fit <- hclust(d, method="complete")
fit$labels
plot(fit) 
plot(fit, hang=-1)
groups <- cutree(fit, k=5) 
class(groups)
rect.hclust(fit, k=5, border="red")


membership<-as.matrix(groups)
table(membership)

final <- data.frame(input, membership)
View(final)




