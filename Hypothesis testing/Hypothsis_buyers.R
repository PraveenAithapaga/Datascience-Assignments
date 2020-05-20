library(readr)
BuyerRatio <- read.csv(file.choose(),header = T)

View(BuyerRatio)
stacked_cof<-stack(BuyerRatio)
attach(stacked_cof)
View(stacked_cof)
table(stacked_cof$ind,stacked_cof$values)
chisq.test(table(stacked_cof$ind,stacked_cof$values))

# p-value = 0.297> 0.05 accept null hypothesis