sal_data <- read.csv(file.choose(),header = T)
View(sal_data)
summary(sal_data)

install.packages("lattice")
library("lattice")

dotplot(sal_data$YearsExperience,main= "dotplot of years of exp")
dotplot(sal_data$Salary, main= "dotplot of salary")

boxplot(sal_data$YearsExperience,col = "Red")
boxplot(sal_data$YearsExperience,horizontal = T)
boxplot(sal_data$Salary)
boxplot(sal_data$Salary,horizontal = T)


hist(sal_data$YearsExperience)
hist(sal_data$Salary)

qqnorm(sal_data$YearsExperience)
qqline(sal_data$YearsExperience)
qqnorm(sal_data$Salary)
qqline(sal_data$Salary)

hist(sal_data$YearsExperience,probability = T)
lines(density(sal_data$YearsExperience))
lines(density(sal_data$YearsExperience, adjust = 2),lty = "dotted")

hist(sal_data$Salary,probability = T)
lines(density(sal_data$Salary))
lines(density(sal_data$Salary,adjust = 2),lty = "dotted")

# scatter plot
plot(sal_data$YearsExperience,sal_data$Salary)
cor(sal_data$YearsExperience,sal_data$Salary)




reg <- lm(Salary~YearsExperience,data = sal_data)
View(reg)
summary(reg)
confint(reg,level = 0.95)
pred <- predict(reg,interval = "predict")
pred <- as.data.frame(pred)
view(pred)
cor(pred$fit,sal_data$Salary)

sqrt(mean(reg$residuals*reg$residuals)) # rmse


reg1 <- lm(Salary~log(YearsExperience),data = sal_data)
summary(reg1)
confint(reg1,level = 0.95)
pred1<- predict(reg1,interval = "predict")
pred1 <- as.data.frame(pred1)
cor(pred1$fit,sal_data$Salary)

sqrt(mean(reg1$residuals*reg1$residuals)) #rmse 1



reg2 <- lm(log(Salary)~YearsExperience,data = sal_data)
summary(reg2)
confint(reg2, level = 0.95)
pred2 <- predict(reg2,interval = "predict")
pred2 <- as.data.frame(pred2)
cor(pred2$fit,sal_data$Salary)

sqrt(mean(reg2$residuals*reg2$residuals))   #rmse 2


reg3 <- lm(Salary~ sqrt(YearsExperience),data = sal_data)
summary(reg3)
confint(reg3,level = 0.95)
pred3 <- predict(reg3,interval = "confidence")
pred3 <- as.data.frame(pred3)
cor(pred3$fit,sal_data$Salary)

sqrt(mean(reg3$residuals*reg3$residuals))   #rmse3


