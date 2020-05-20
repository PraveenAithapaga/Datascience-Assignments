airlines <- read.csv(file.choose(),header = T)
View(airlines)
plot(airlines)
plot(airlines[,2])
airlinests<- ts(airlines[,2],start = c(1995,1),frequency = 12)
airlinests

library(data.table)
library(ggplot2)
library(fpp2)
library(forecast)
library(stats)
library(tseries)


plot(airlinests, xlab= "years",ylab="passengers",main = "airline passengers")+ggtitle("seasonal plot:airline passenger data")

ggseasonplot(airlinests,year.labels = TRUE, year.labels.left = TRUE)+ylab("degree")+ggtitle("seasonal plot:airlines passengers data")

ggseasonplot(airlinests,polar = TRUE)+ylab("degree")+ggtitle("seasonal plot: airlines passengers data ")

airdec <- decompose(airlinests,type = "multiplicative")
airdec

airstl <- stl(log10(airlinests),s.window = "p")
plot(airstl)

air_train <- window(airlinests, start=c(1995,1), end=c(2000,12),frequency = 12)
air_train

air_test <- window(airlinests,start= c(2001,1), end= c(2002,12), frequency =12)
air_test

airdec_train_log<- stl(log10(air_train),s.window="p")
airdec_train_log

air_trainstl <- forecast(airdec_train_log,method = "rwdrift",h=24)
plot(air_trainstl)

vec2 <- 10^(cbind(log10(air_test), as.data.frame(forecast(airdec_train_log, method = "rwdrift", h=24))[,1]))

ts.plot(vec2, col=c("blue","red"),main= "airlines passengers : actual vs forecast")            

RMSE2 <- round(sqrt(sum(((vec2[,1]-vec2[,2])^2)/length(vec2[,1]))),4)
mape2 <- round(mean(abs(vec2[,1]-vec2[,2])/vec2[,1]),4)
vec2
paste("accuracy measures:RSME",RMSE2,"and MAPE:",mape2)
