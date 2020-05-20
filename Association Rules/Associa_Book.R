install.packages("arulesViz")
library(arules)
library(arulesViz)
book<-read.csv(file.choose(),header = T)
View(book)
class(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])

rules<-apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.7))

inspect(rules[1:100])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")


rules<-apriori(as.matrix(book),parameter = list(support=0.005,confidence=0.75)) 
#when support support=0.002,confidence=0.7

inspect(rules[1:100])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")


rules<-apriori(as.matrix(book),parameter = list(support=0.01,confidence=0.80)) 
#when support support=0.01,confidence=0.80
inspect(rules[1:100])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")
