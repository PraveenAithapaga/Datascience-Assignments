mydata<-read.csv(file.choose(),header = T)
colnames(mydata)
View(mydata)


Sales_Result <- NULL
Sales_Result <- ifelse(mydata$Sales > 7.490,1,0)
mydata[,"Sales_Result"] <- Sales_Result

mydata$ShelveLoc <- as.factor(mydata$ShelveLoc)
View(mydata$ShelveLoc)
mydata$Urban <- as.factor(mydata$Urban)
mydata$US <- as.factor(mydata$US)
mydata$Sales_Result <- as.factor(mydata$Sales_Result)

sales_high <- mydata[mydata$Sales_Result == "1",] 
sales_low <- mydata[mydata$Sales_Result == "0",]

data_train <- rbind(sales_high[1:150,], sales_low[1:150,])
data_test <- rbind(sales_high[151:199,], sales_low[151:201,])
install.packages("randomForest")
library(randomForest)
fit.forest <- randomForest(Sales_Result~.,data=data_train, na.action=na.roughfix,importance=TRUE)

mean(data_train$Sales_Result == predict(fit.forest,data_train))


pred_train <- predict(fit.forest,data_train)
library(caret)

confusionMatrix(data_train$Sales_Result, pred_train)


pred_test <- predict(fit.forest,newdata=data_test)
mean(pred_test == data_test$Sales_Result)

confusionMatrix(data_test$Sales_Result, pred_test)



plot(fit.forest,lwd=2)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)
