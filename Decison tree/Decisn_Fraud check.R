fraud <- read.csv(file.choose(),header = T)
View(fraud)
rsky_good=ifelse(fraud$Taxable.Income>= 30000,"good","risky")
fraud_data=data.frame(fraud,rsky_good)
View(fraud_data)
str(fraud_data)


train_fraud <- fraud_data[1:300,]
test_fraud <- fraud_data[301:600,]

library(tree)
fd_tree_org <- tree(rsky_good~.-Taxable.Income,data = fraud_data)
summary(fd_tree_org)

fd_tree <- tree(rsky_good~.-Taxable.Income,data = train_fraud)
summary(fd_tree)
plot(fd_tree)
text(fd_tree,pretty = 0)


pred_tree <- as.data.frame(predict(fd_tree,newdata = test_fraud))
pred_tree["final"]<- NULL
pred_tree_df <-predict(fd_tree,newdata = test_fraud)
pred_tree$final <- colnames(pred_tree_df)[apply(pred_tree_df,1,which.max)]
pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(test_fraud$rsky_good)
mean(pred_tree$final==fraud_data$rsky_good)

library(gmodels)
CrossTable(test_fraud$rsky_good,pred_tree$final)

confusionMatrix(test_fraud$rsky_good,pred_tree$final)
