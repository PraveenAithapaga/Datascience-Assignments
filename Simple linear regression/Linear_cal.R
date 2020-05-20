cal_data <- read.csv(file.choose(),header = T)
View(cal_data)
summary(cal_data)
install.packages("lattice")
library("lattice")
names(cal_data)
colnames(cal_data) <- c("wt","cl")
names(cal_data)

dotplot(cal_data$wt)
dotplot(cal_data$cl)

boxplot(cal_data$wt)
boxplot(cal_data$wt,horizontal = T)
boxplot(cal_data$cl)
boxplot(cal_data$cl,horizontal = T)

hist(cal_data$wt)
hist(cal_data$cl)

qqnorm(cal_data$wt)
qqline(cal_data$wt)
qqnorm(cal_data$cl)
qqline(cal_data$cl)

hist(cal_data$wt,probability = T)
lines(density(cal_data$wt))
lines(density(cal_data$wt, adjust = 2),lty = "dotted")

hist(cal_data$cl,probability = T)
lines(density(cal_data$cl))
lines(density(cal_data$cl,adjust = 2),lty = "dotted")

# scatter plot
plot(cal_data$wt,cal_data$cl)
cor(cal_data$wt,cal_data$cl)



reg <- lm(wt~cl,data = cal_data)
View(reg)
summary(reg)
confint(reg,level = 0.95)
pred <- predict(reg,interval = "predict")
pred <- as.data.frame(pred)
View(pred)
cor(pred$fit , cal_data$wt) #cor = 0.94

sqrt(mean(reg$residuals*reg$residuals))  #rmse




reg1 <- lm(wt~log(cl),data = cal_data)
summary(reg1)
pred1 <- predict(reg1 , interval = "predict")
pred2 <- as.data.frame(pred1)
cor(pred2$fit,cal_data$wt) # cor = 0.89

sqrt(mean(reg1$residuals*reg1$residuals))   #rmse 1

reg2 <- lm(log(wt)~cl,data = cal_data)
summary(reg2)
pred3 <- predict(reg2,interval = "predict")
pred3 <- as.data.frame(pred3)

cor(pred3$fit, cal_data$wt)  # 0.946

sqrt(mean(reg2$residuals*reg2$residuals)) #rmse 2

reg3 <- lm(wt~ sqrt(cl), data= cal_data)
summary(reg3)
pred4 <- predict(reg3,interval = "confidence")
pred4 <- as.data.frame(pred4)

cor(pred4$fit, cal_data$wt)  # 0.92

sqrt(mean(reg3$residuals*reg3$residuals))  #rmse 3




