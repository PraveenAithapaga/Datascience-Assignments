
forestfires <- read.csv(file.choose(),header = T)
View(forestfires)
str(forestfires)
attach(forestfires)
month<-as.numeric(forestfires$month)
forestfires<-forestfires[,-1]
forestfires<-cbind(forestfires,month)
day<-as.numeric(forestfires$day)
forestfires<-forestfires[,-1]
forestfires<-cbind(forestfires,day)
size_category<-as.numeric(forestfires$size_category)
forestfires<-forestfires[,-29]
forestfires<-cbind(forestfires,size_category)




normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires,FUN=normalize))
summary(forestfires_norm$temp)
summary(forestfires$temp)
forestfires_train<-forestfires_norm[1:400,]
forestfires_test<-forestfires_norm[401:517,]


install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

forestfires_model <- neuralnet(temp ~ FFMC+DC+ISI+RH+wind+area+rain+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthmay+monthnov+monthnov+monthoct+monthsep,data = forestfires_train)
str(forestfires_model)
plot(forestfires_model)

model_results <- compute(forestfires_model,forestfires_test[1:30])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,forestfires_test$temp)
plot(predicted_strength,forestfires_test$temp)
model_5<-neuralnet(temp ~ FFMC+DC+ISI+RH+wind+area+rain+dayfri+daymon+daysat+daysun+daythu+daytue+daywed+daywed+monthapr+monthaug+monthdec+monthfeb+monthjan+monthjul+monthjun+monthmar+monthmay+monthmay+monthnov+monthnov+monthoct+monthsep,data= forestfires_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,forestfires_test[1:30])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forestfires_test$temp)
plot(pred_strn_5,forestfires_test$temp)
