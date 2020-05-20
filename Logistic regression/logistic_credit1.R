data <- read.csv(file.choose(),header = T)
View(data) 
str(data)
summary(data)

data1 <- data[,-1]
View(data1)
data2 <- data1[,-5]
View(data2)
data3<- data2[,-5]
View(data3)
summary(data3)


str(data3)


data3$owner <- ifelse(data3$owner == "yes",1,0)
data3$owner <- as.factor(data3$owner)

data3$selfemp <- ifelse(data3$selfemp == "yes",1,0)
data3$selfemp <- as.factor(data3$selfemp)

data3$majorcards <- as.factor(data3$majorcards)
str(data3)

is.na(data3)


prop.table(table(data3$card))

boxplot(data3$age)
boxplot(data3$age, horizontal = T)

boxplot(data3$income)



xtabs(~card+majorcards, data=data3)
xtabs(~card+selfemp , data = data3)
xtabs(~card+owner , data = data3)

logistic <- glm(card ~ . , data = data3, family = "binomial")
summary(logistic)

ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null


predicted.data <- data.frame(
  probability.of.card=logistic$fitted.values,
  card=data3$card)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.card, decreasing=FALSE),]
View(predicted.data)
predicted.data$rank <- 1:nrow(predicted.data)
View(predicted.data$probability.of.card)

library(ggplot2)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.card)) +
  geom_point(aes(color=card), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting card")



# Confusion matrix table 
prob <- predict(logistic,type=c("response"),data3)
prob
confusion<-table(prob>0.5,data3$card)
confusion

# Model Accuracy 
Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy
Error <- 1-Accuracy
Error

# ROC Curve 
install.packages("ROCR")
library(ROCR)
rocrpred<-prediction(prob,data3$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T,text.adj=c(-0.2,1.7))


