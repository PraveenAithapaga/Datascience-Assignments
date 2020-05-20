forest <- read.csv(file.choose(),header = T)
library(caret)
str(forest)
View(forest)
forest1 <- forest[,3:9]

normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  
}

mydata$temp <- normalise(mydata$temp)
mydata$rain <- normalise(mydata$rain)
mydata$RH <- normalise(mydata$RH)
mydata$wind <- normalise(mydata$wind)

View(forest1)
str(forest1)


train1 <- forest1[1:300,]
View(train1)
test1<- forest1[301:517,]

model1<-ksvm(size_category ~.,data = train1,kernel = "vanilladot")
model1


model_rfdot<-ksvm(size_category ~.,data = train1,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test1)
mean(pred_rfdot== test1$size_category)
View(pred_rfdot)

model_vanilla<-ksvm(size_category ~.,data =train1,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test)
mean(pred_vanilla== test1$size_category) 


model_besseldot<-ksvm(size_category ~.,data = train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test)
mean(pred_bessel==test1$size_category)


model_poly<-ksvm(Salary ~.,data = train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test)
mean(pred_polyl==test$Salary)






