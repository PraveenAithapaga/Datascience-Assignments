Startups <- read.csv(file.choose(),header = T)
View(Startups)
str(Startups)
attach(Startups)
State<-as.numeric(Startups$State)
Startups<-Startups[,-4]
Startups<-cbind(Startups,State)
str(Startups)
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
Startups_norm<-as.data.frame(lapply(Startups,FUN=normalize))
summary(Startups_norm$Profit)
summary(Startups$Profit)
Startups_train<-Startups_norm[1:35,]
Startups_test<-Startups_norm[35:50,]

install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

Startups_model <- neuralnet(Profit~ R.D.Spend+Administration+Marketing.Spend,data = Startups_train)
str(Startups_model)
plot(Startups_model)

model_results <- compute(Startups_model,Startups_test[1:4])
predicted_strength <- model_results$net.result
predicted_strength
model_results$neurons
cor(predicted_strength,Startups_test$Profit)
plot(predicted_strength,Startups_test$Profit)
model_5<-neuralnet(Profit~ R.D.Spend+Administration+Marketing.Spend,data= Startups_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,Startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,Startups_test$Profit)
plot(pred_strn_5,Startups_test$Profit)
