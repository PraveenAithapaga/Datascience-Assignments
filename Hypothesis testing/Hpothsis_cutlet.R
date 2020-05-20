
cutlets <- read.csv(file.choose(),header = T)
View(cutlets)


attach(Cutlets)


shapiro.test(Cutlets$`Unit A`)

shapiro.test(Cutlets$`Unit B`)


var.test(Cutlets$`Unit A`,Cutlets$`Unit B`)


t.test(Cutlets$`Unit A`,Cutlets$`Unit B`,alternative = "two.sided",conf.level = 0.95,correct = TRUE)#two sample T.Test


t.test(Cutlets$`Unit A`,Cutlets$`Unit B`,alternative = "greater",var.equal = T)


# p-value = 0.2361 > 0.05 => accept Null Hypothesis
#their is no difference between the dia of cutlets