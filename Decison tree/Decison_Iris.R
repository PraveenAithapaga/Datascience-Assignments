data()
data("iris")
View(iris)
install.packages("C50")
install.packages("tree")
library(tree)
library(C50)

table(iris$Species)
iris_setosa<- iris[iris$Species=="setosa",]
iris_versicolor<-iris[iris$Species=="versicolor",]
iris_virginca<-iris[iris$Species=="virginica",]

View(iris_setosa)
iris_train<- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginca[1:25,])
View(iris_train)
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginca[26:50,])

irisc5.0_train<- C5.0(iris_train[,-5],iris_train$Species)
View(irisc5.0)
plot(irisc5.0)

mean(iris_train$Species==predict(irisc5.0,iris_train))
predictc5.0_test <- predict(irisc5.0_train,newdata=iris_test)
mean(predictc5.0_test==iris_test$Species)
library(gmodels)
CrossTable(iris_test$Species,predictc5.0_test)
