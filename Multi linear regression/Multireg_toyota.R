toy <- read.csv(file.choose(),header = T)
Cor<-toy[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Cor)
attach(Cor)
qqnorm(HP)
qqline(HP)
pairs(Cor)
cor(Cor)
model.cor <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(model.cor)
model.corC <- lm(Price ~ cc)
summary(model.corC)
model.corD <- lm(Price ~ Doors)
summary(model.corD)
model.corCD <- lm(Price ~ cc+Doors)
summary(model.corCD)
cor(cc,Doors)
install.packages("corpcor")
library(corpcor)
cor(Cor)
cor2pcor(cor(Cor))

install.packages("car")
library(car)
plot(model.cor)
qqplot(model.cor,id.n=5)


influence.measures(model.cor)
influenceIndexPlot(model.cor,id.n=3)
influencePlot(model.cor,id.n=3)


model.cor1 <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Cor[-81,])
summary(model.cor1)

model.cor2 <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight,data = Cor[-961,])
summary(model.cor2)
,data = Cor[-222,])
Summary(model.cor3)
model.cor3 <- lm(Price ~ Age_08_04+KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight

variance inflation 

vif(model.cor)
vifage <- lm(Age_08_04 ~ KM+HP+cc+Doors+Gears+Quarterly_Tax+Weight )
summary(vifage)
vifKM <- lm(KM ~ Age_08_04+HP+cc+Doors+Gears+Quarterly_Tax+Weight)
summary(vifKM)


avPlots(model.cor,id.n = 2, id.cex=0.8,col="red")

library("MASS")
stepAIC(model.cor)

model.final <- lm(Price~Age_08_04+KM+HP+Gears+Quarterly_Tax+Weight)
summary(model.final)
