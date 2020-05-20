crime_dat<-read.csv(file.choose(),header = T)
View(crime_dat)

crime_data<-crime_dat[	,2:5]
View(crime_data)

norm_data<-scale(crime_data)

d<- dist(norm_data,method = "euclidean")
View(d)

fit<-hclust(d,method = "complete")
plot(fit)	
plot(fit,hang = -1)

groups<-cutree(fit,k=5)

membership<-as.matrix(groups)	
View(membership)	

final<-data.frame(crime_dat,membership)
View(final)	
