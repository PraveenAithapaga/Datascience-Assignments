st_data <- read.csv(file.choose(),header = T)
View(st_data)
st_data1 <- st_data[,-4]
View(st_data1)
attach(st_data1)
qqnorm(R.D.Spend)
qqline(R.D.Spend)
qqnorm(Administration)
qqline(Administration)
qqnorm(Marketing.Spend)
qqline(Marketing.Spend)
pairs(st_data1)
cor(st_data1)
model.st <- lm(Profit~ R.D.Spend+Administration+Marketing.Spend)
summary(model.st)

model.stA <- lm(Profit ~ Administration)
summary(model.stA)

model.stM <- lm(Profit ~ Marketing.Spend)
summary(model.stM)

model.stAM <- lm(Profit~ Administration+Marketing.Spend)
summary(model.stAM)


install.packages("corpcor")
library(corpcor)

cor(st_data1)
install.packages("cars")
library(car)
plot(model.st)
qqplot(model.st,id.n = 5)


influence.measures(model.st)
influeneindexplot(model.st,id.n=3)
influenceplot(model.st)
influencePlot(model.st)

model.st1 <- lm(Profit~ R.D.Spend+Administration+Marketing.Spend,data = st_data[-c(46,47,49,50),])
summary(model.st1)
summary(model.st1)                

vif(model.st)
vifA <- lm(Administration~ R.D.Spend+Marketing.Spend)
summary(vifA)

vifR <- lm(R.D.Spend ~Administration+Marketing.Spend)
summary(vifR)

avPlots(model.st,id.n=2,id.cex=0.8,col="red")
library("MASS")
stepAIC(model.st)

model.final <- lm(Profit~R.D.Spend+Marketing.Spend)
summary(model.final)

