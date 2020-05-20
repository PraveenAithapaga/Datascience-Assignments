zoo <- read.csv(file.choose(),header = T)
View(zoo)
str(zoo)


zoo1 <- zoo[-1]
str(zoo1)
head(zoo1)

normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
zoo.n <- as.data.frame(lapply(zoo1[,1:16],normalize))
summary(zoo.n$legs)


set.seed(123)
dat.d <- sample(1:nrow(zoo.n),size = nrow(zoo.n)*0.7,replace = FALSE) 
train.zoo <- zoo1[dat.d,]
test.zoo <- zoo1[-dat.d,]

tarin.zoo_labels <- zoo1[dat.d,1]
test.zoo_labels <- zoo1[-dat.d,1]


install.packages("class")
library(class)
NROW(tarin.zoo_labels)
sqrt(70)
knn7 <- knn(train = train.zoo,test = test.zoo, cl=tarin.zoo_labels,k=7)
knn8 <- knn(train = train.zoo, test = test.zoo, cl=tarin.zoo_labels,k=8)
knn9 <- knn(train = train.zoo,test = test.zoo,cl=tarin.zoo_labels,k=9)
acc7 <- 100*sum(test.zoo_labels == knn7)/NROW(test.zoo_labels)
acc7
acc8 <- 100*sum(test.zoo_labels == knn8)/NROW(test.zoo_labels)

knn11 <- knn(train = train.zoo, test = test.zoo, cl=tarin.zoo_labels,k=11)
acc11 <- 100*sum(test.zoo_labels == knn11)/NROW(test.zoo_labels)

table(knn9,test.zoo_labels) 

install.packages("caret")
library(caret)

confusionMatrix(table(knn9,test.zoo_labels))
