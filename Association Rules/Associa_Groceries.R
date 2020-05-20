
library(arules)
library(arulesViz)

groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)
attach(groceries)

groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))
inspect(groceries_rules[1:10])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")


groceries_rules1<-apriori(groceries,parameter = list(support = 0.001,confidence = 0.01,minlen=4))
inspect(groceries_rules1[1:10])
plot(rules,method = "scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph")
plot(rules,method = "mosaic")
