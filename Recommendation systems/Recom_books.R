install.packages("recommenderlab", dependencies=TRUE)
install.packages("Matrix")
library("recommenderlab")
library(caTools)

books_data <- read.csv(file.choose(),header = TRUE)
View(books_data)
books_data <- books_data[,2:4]
class(books_data)
View(books_data)
str(books_data)


books_data_matrix <- as(books_data, 'realRatingMatrix')
View(books_data_matrix)

books_recomm_model1 <- Recommender(books_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items1 <- predict(books_recomm_model1, books_data_matrix[413:414], n=5)
as(recommended_items1, "list")


## Popularity model recommends the same movies for all users , we need to improve our model using # # Collaborative Filtering

#User Based Collaborative Filtering

books_recomm_model2 <- Recommender(books_data_matrix, method="UBCF")

#Predictions for two users 
recommended_items2 <- predict(books_recomm_model2, books_data_matrix[413:414], n=10)
as(recommended_items2, "list")
