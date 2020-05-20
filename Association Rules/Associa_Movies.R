library(arules)
library(arulesViz)

movies<-read.csv(file.choose(),header = T)
View(movies)
class(movies)
movies_new<-movies[,-(1:5)]
View(movies_new)
movies_trans<-as(as.matrix(movies_new),"transactions")
inspect(movies_trans[1:10])
attach(movies_new)

rules<-apriori(as.matrix(movies_new),parameter = list(support=0.002,confidence=0.7))
inspect(rules[1:10])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")


rules1<-apriori(as.matrix(movies_new),parameter = list(support=0.01,confidence=0.9))
inspect(rules1[1:10])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")
