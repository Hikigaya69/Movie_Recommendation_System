#MovieLens Recommendation
#MovieLens Data


#library importing 
library(recommenderlab) #recommendation
library(reshape2)
library(data.table)
library(ggplot2) #visualization


#the data
movie_data <- read.csv("movies.csv", stringsAsFactors = FALSE)
rating_data <- read.csv("ratings.csv")


str(movie_data)
str(rating_data)


data.table(movie_data)  #id,title,genres
data.table(rating_data)


#summary statistics
summary(movie_data)
summary(rating_data)



movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors = FALSE)
movie_genre
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], "[|]", type.convert = TRUE),stringsAsFactors = FALSE)



colnames(movie_genre2) <-c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_matl <- matrix(0,10330,18)
genre_matl[1,] <- list_genre
colnames(genre_matl) <- list_genre

for (index in 1:nrow(movie_genre2)){
  for(col in 1:ncol(movie_genre2)){
    gen_col = which(genre_matl[1,] == movie_genre2[index,col])
    genre_matl[index+1,gen_col] <- 1
  }
}


genre_mat2 <- as.data.frame(genre_matl[-1,], stringsAsFactors=FALSE)

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col])
}

str(genre_mat2)



#  search matrix that gives us films based on genres

SearchMovie <- cbind(movie_data[,1:2],genre_mat2[])

head(SearchMovie)



# sparse matrix for recommendation

ratingMatrix <- dcast(rating_data, userId~movieId, value.var = "rating", na.rm=FALSE) #basically our sparse matrix
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")


#recommendation model
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(recommendation_model)



lapply(recommendation_model, "[[", "description")
#item based collaborative filtering

recommendation_model$IBCF_realRatingMatrix$parameters


#similarity
similarity_mat <- similarity(ratingMatrix[1:4, ],method = "cosine",which="users")

as.matrix(similarity_mat)

image(as.matrix(similarity_mat), main = "User's Similarity")
# similarity of movies
movie_similarity<- similarity(ratingMatrix[ ,1:4],method = "cosine",which="items")

as.matrix(movie_similarity)

image(as.matrix(movie_similarity), main = "movie Similarity")

#rating values

rating_values<-as.vector(ratingMatrix@data)
unique(rating_values)

table_rating<-table(rating_values)
table_rating

#most viewd movies

movie_views<-colCounts(ratingMatrix) #counts for each movie

table_views<-data.frame(movie=names(movie_views),
              views=movie_views    )
table_views<-table_views[order(table_views$views,decreasing = TRUE),]
table_views$title<-NA
for (index in 1:10325){
  table_views[index,3]<-as.character(subset(movie_data,movie_data$movieId==table_views[index,1])$title)
}
table_views[1:6,]

ggplot(table_views[1:6,],aes(x=title,y=views))+
  geom_bar(stat = "identity",fill="steelblue")+
geom_text(aes(label="views"),vjust=0.3,size=3.5)+
  theme(axis.text.x = element_text(angle = 45,hjust=1))+
  ggtitle("Total views of the top films")

#heatmap
image(ratingMatrix[1:50,1:50],axes=FALSE,main="30 X 30 heatmap")

#select useful data
#normalize
#binarize ir

movie_ratings<-ratingMatrix[rowCounts(ratingMatrix)>50,
                            colCounts(ratingMatrix)>50]

movie_ratings
#heatmap of top users and movies
minimum_movies<-quantile(rowCounts(movie_ratings),0.98)
minimum_users<-quantile(colCounts(movie_ratings),0.98)
image(movie_ratings[rowCounts(movie_ratings)>minimum_movies,colCounts(movie_ratings)>minimum_users],
      main="Heatmap of top users and movies")




average_ratings<-rowMeans(movie_ratings)
qplot(average_ratings,fill=I("steelblue"),col=I("red"))+
  ggtitle("Distribution of average rating per user")



normalized_ratings<-normalize(movie_ratings)
sum(rowMeans(normalized_ratings)>0.00001)

image(normalized_ratings[rowCounts(normalized_ratings)>minimum_movies,colCounts(normalized_ratings)>minimum_users],
      main="normalized ratings of top users")

#binarize movie is recommended if the value is more than 3.5
binary_minimiummovies<-quantile(rowCounts(movie_ratings),0.90)
binary_minimiumusers<-quantile(colCounts(movie_ratings),0.90)

movies_watched<-binarize(movie_ratings,minRating=1)


goodratedfilms<-binarize(movie_ratings,minRating=3.5)


image(goodratedfilms[rowCounts(movie_ratings)>binary_minimiummovies,
                     colCounts(movie_ratings)>binary_minimiumusers],
      main="heatmap of the top users and movies")


#collaborative system

sampled_data<-sample(x=c(TRUE,FALSE),
                     size=nrow(movie_ratings),replace = TRUE,
                     prob=c(0.8,0.2))
training_data<-movie_ratings[sampled_data,]
testing_data<-movie_ratings[sampled_data,]

#recommendation system
recommendation_system<-recommenderRegistry$get_entries(dataType="realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommen_model<-Recommender(data <-training_data,
                            method="IBCF",
                            parameter=list(k=50))
recommen_model

class(recommen_model)
info<-getModel(recommen_model)

class(info$sim)
top_items<-50
image(info$sim[1:top_items,1:top_items],
      main="Heatmap of the fist row and first columns")

sum_rows<-rowSums(info$sim>0)
table(sum_rows)
sum_cols<-colSums(info$sim>0)
qplot(sum_cols,fill= I("steelblue"),col=I("red"))+ggtitle("Distribution of the column count")

top_recommendations<-30 #no of items recommended to each user

predictedrecommendations<-predict(object = recommen_model,
                                  newdata=testing_data,
                                  n=top_recommendations)






predictedrecommendations






user4<-predictedrecommendations@items[[4]]
movie_user4<-predictedrecommendations@itemLabels[user4]
movie_user5<-movie_user4
for(index in 1:30)
{
  movie_user5[index]<-as.character(subset(movie_data,movie_data$movieId==
                                            movie_user5[index])$title)
}
movie_user5

user3<-predictedrecommendations@items[[3]]
movie_user3<-predictedrecommendations@itemLabels[user3]
movie_user4<-movie_user3
for(index in 1:30)
{
  movie_user4[index]<-as.character(subset(movie_data,movie_data$movieId==
                                            movie_user3[index])$title)
}


movie_user4


#recommder matrix



