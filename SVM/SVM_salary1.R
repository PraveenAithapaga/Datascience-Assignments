train1 <- read.csv(file.choose(),header = T)
test1 <- read.csv(file.choose(),header= T)
train <- train1
test <- test1

View(train)
View(test)

str(train)
class(train)

ggplot(data = train,aes(x=train$Salary,y = train$age ,fill = train$Salary))+geom_boxplot()+
  ggtitle("Box Plot")

plot(train$workclass,train$Salary)
plot(train$education,train$Salary)
plot(train$maritalstatus,train$Salary)

ggplot(data=train,aes(x=train$Salary, y = train$capitalgain, fill = train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train,aes(x=train$Salary, y = train$hoursperweek, fill = train$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

ggplot(data=train,aes(x = train$age, fill = train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train, aes(x = train$workclass, fill = train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')

ggplot(data=train,aes(x = train$education, fill = train$Salary)) +
  geom_density(alpha = 0.9, color = 'Violet')




install.packages("caret")
install.packages("kernlab")
library(kernlab)
library(caret)


model1<-ksvm(Salary ~.,data = train1,kernel = "vanilladot")
model1


model_rfdot<-ksvm(Salary ~.,data = train1,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test1)
mean(pred_rfdot== test$Salary)
View(pred_rfdot)

model_vanilla<-ksvm(Salary ~.,data =train1,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test)
mean(pred_vanilla== test$Salary) 


model_besseldot<-ksvm(Salary ~.,data = train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test)
mean(pred_bessel==test$Salary)


model_poly<-ksvm(Salary ~.,data = train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test)
mean(pred_polyl==test$Salary)





