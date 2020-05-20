
library(dplyr)
library(tree)
 
compdata <- read.csv(file.choose(),header = T)
View(compdata)
mean(compdata$Sales)
hist(cd$Sales)


high = ifelse(compdata$Sales<10,"no","yes")
cd = data.frame(compdata,high)
View(cd)
str(cd)

table(cd$high)
prop.table(table(cd$high))


# In order to get the same proption split of unbalenced dataset

library(caTools)
set.seed(101)
split_tag <- sample.split(cd$high,SplitRatio = 0.65)
train <- subset(cd,split_tag == TRUE)
test <- subset(cd,split_tag == FALSE)
View(train)
prop.table(table(train$high))

tree1 <- tree(high~.-Sales,data = train)
summary(tree1)
plot(tree1)
text(tree1,pretty = 0)

tree.pred = predict(tree1,test,type = "class")
table(tree.pred,test$high)

 
tree2 <- tree(high~.-(Sales+Urban+US),data = train)
summary(tree2)
plot(tree2)
text(tree2,pretty = 0)

tree.pre = predict(tree2,test,type = "class")
table(tree.pre , test$high)

#PRUNING

#Cross validation

cv.comp = cv.tree(tree1, FUN = prune.misclass)
cv.comp
plot(cv.comp)


#pruning1
prune.comp = prune.misclass(tree1,best = 15)
plot(prune.comp)
text(prune.comp,pretty = 0)
tree.pred1 = predict(prune.comp,test,type = "class")
table(test$high,tree.pred1)
(106+16)/(106+16+6+12)

#pruning 2 
prune.comp1 = prune.misclass(tree1,best = 8)
plot(prune.comp1)
text(prune.comp1,pretty = 0)
tree.pred2 = predict(prune.comp1,test,type = "class")
table(test$high,tree.pred2)
(100+17)/(100+17+11+12)

#pruning 3
prune.comp2 = prune.misclass(tree1,best = 10)
plot(prune.comp2)
text(prune.comp2,pretty = 0)
tree.pred3 = predict(prune.comp2,test,type = "class")
table(test$high,tree.pred3)
(103+17)/(103+17+9+11)


