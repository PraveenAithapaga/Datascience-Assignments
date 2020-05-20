data <- read.table(file.choose(), sep = ";",header=TRUE)
View(data)
str(data)
summary(data)

data$job <- as.factor(data$job)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
data$default  <- as.factor(data$default)
data$housing  <- as.factor(data$housing) 
data$loan <- as.factor(data$loan)
data$contact <-as.factor(data$contact) 
data$month <- as.factor(data$month)
data$poutcome  <- as.factor(data$poutcome)
data$y  <- as.factor(data$y)      


str(data)
data1 <- data[,-12]

logistic <- glm(y~., data = data,family = "binomial")
summary(logistic)



ll.null<- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
(ll.null-ll.proposed)/ll.null


predicted.data <- data.frame(
      probability.of.y = logistic$fitted.values,
      y=data$y)
predicted.data <- predicted.data[
  order(predicted.data$probability.of.y,decreasing = FALSE),]

predicted.data$rank <- 1:nrow(predicted.data)
View(predicted.data$probability.of.y)

library(ggplot2)

ggplot(data=predicted.data, aes(x=rank, y=probability.of.y)) +
  geom_point(aes(color=y), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting y")
