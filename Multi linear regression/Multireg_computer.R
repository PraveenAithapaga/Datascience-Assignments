
Computer_Data <- read.csv(file.choose(),header = T)
View(Computer_Data)
attach(Computer_Data)


summary(Computer_Data)
CD<-as.factor(Computer_Data$cd)
MULTI<- as.factor(Computer_Data$multi)
PREMIUM<-as.factor(Computer_Data$premium)
Computer_Data_New<- Computer_Data [,-c(7,8,9)]
View(Computer_Data_New)
Computer_Data_New<-cbind(Computer_Data_New,CD,MULTI,PREMIUM)
View(Computer_Data_New)
str(Computer_Data_New)

pairs(Computer_Data_New)
plot(Computer_Data_New)
cor(Computer_Data_New)
install.packages("psych")
library(psych)
pairs.panels(Computer_Data_New)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Computer_Data_New))


model.Computer <- lm(price~speed+hd+ram+screen+ads+trend,data=Computer_Data_New)
summary(model.Computer)

model.ComputerS<-lm(price~speed)
summary(model.ComputerS) 

# Prediction based on only HD
model.ComputerH<-lm(price~hd)
summary(model.ComputerH) 

# Prediction based on ads 
model.ComputerA<-lm(price~ads)
summary(model.ComputerA) 

# Prediction based on screen
model.ComputerS<-lm(price~screen)
summary(model.ComputerS)

# Prediction based on RAM
model.ComputerR<-lm(price~ram)
summary(model.ComputerR)

# Prediction based on trend
model.ComputerT<-lm(price~trend)
summary(model.Computer)



library(psych)
pairs.panels(Computer_Data_New)

influence.measures(model.Computer)
library(car)
influenceIndexPlot(model.Computer,id.n=3) 
influencePlot(model.Computer,id.n=3) 

model.Computer11<-lm(price~speed+hd+ram+screen+ads+trend,data=Computer_Data_New[-c(1701,1441,3784,4478),])
summary(model.Computer11)
vif(model.Computer)
avPlots(model.Computer,id.n=2,id.cex=0.7)
finalmodel<-lm(price~speed+hd+ram+screen+ads+trend)
summary(finalmodel)
plot(finalmodel)
qqPlot(model.Computer,id.n = 5)
