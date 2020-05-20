library(e1071)
library(mlbench)
library(ggplot2)
tarin_sal <- read.csv(file.choose(),header = T)
str(tarin_sal) 
View(tarin_sal)
tarin_sal $educationno <- as.factor(tarin_sal$educationno)

test_sal<- read.csv(file.choose(),header = T)
str(test_sal)
test_sal$educationno <- as.factor(test_sal$educationno)

ggplot(data=tarin_sal,aes(x=tarin_sal$Salary, y = tarin_sal$age, fill = tarin_sal$Salary)) +
  geom_boxplot() +
  ggtitle("Box Plot")

model <- naiveBayes(tarin_sal$Salary ~ . , data = tarin_sal)
model

model_pred <-  predict(model,test_sal)
mean(model_pred== test_sal$Salary)
install.packages("psych")
library(psych)
library(caret)
confusionMatrix(model_pred,test_sal$Salary)
