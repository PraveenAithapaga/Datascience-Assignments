
Faltoons <- read.csv(file.choose(),header = T)
View(Faltoons)

attach(Faltoons)
table1 <- table(Weekdays,Weekend)
table1
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")

prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "less")


chisq.test(table(Weekdays,Weekend))
