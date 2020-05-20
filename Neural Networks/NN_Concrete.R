
concrete <- read.csv(file.choose(),header = T)
View(concrete)
str(concrete)
attach(concrete)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete,FUN=normalize))
summary(concrete_norm$strength)
summary(concrete$strength)
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
str(concrete_model)
plot(concrete_model)


model_results <- compute(concrete_model,concrete_test[1:8])
predicted_strength <- model_results$net.result

predicted_strength
model_results$neurons
cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)
model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)
plot(pred_strn_5,concrete_test$strength)
