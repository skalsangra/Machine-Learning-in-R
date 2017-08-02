#1-Data Acquisition
#Load Movie dataset of IMDB
movie_data <- read.csv(file = "D:/R Docs/ML code/movie_metadata.csv")
#summary(movie_data)
#head(movie_data)
movie<- data.matrix(movie_data)

#removing NULL values
movie <- na.omit(movie)

#we cannot use factors variable in clustering like names or any yes/no type variable

#selecting a random sample of 500 values,dont use 'sample' as it is keyword in R
smple <- movie[sample(nrow(movie),500),]

#####################################################################################
#**********************Hierarchical clustering things********************************
#Normalization
#lets here we take c9-gross and c28-no. of facebook likes
#now since values of both variable does not have a common base or range 
#we can normalize them to provide a common range of values
z <- smple[ ,c(9,28)]
m <- apply(z, 2, mean)
s <- apply(z, 2, sd)
z <- scale(z, m, s)

#calculating Euclidean distances between variables
z <- z[sample(nrow(z),20),] #selecting only 20 rows out of 500
eucli_dist <- dist(z)
print(eucli_dist, digits = 2)

#Hierarchical clustering
#making cluster dendrogram with "complete" linkage method
hc_model_cl <- hclust(eucli_dist)
plot(hc_model_cl)

#making cluster dendrogram with "average" linkage method
hc_model_al <- hclust(eucli_dist, method = 'average')
plot(hc_model_al)

#cluster membership, lets divide into 3 clusters all elements of above trees
member_cl <- cutree(hc_model_cl, 3)
member_al <- cutree(hc_model_al, 3)
table(member_al, member_cl)
#*************************************************************************************
######################################################################################

#selecting column c9 and c23 for cluster analysis
short_smple <- smple[,c(9,23)]
short_matrix <- data.matrix(short_smple)
plot(short_matrix)


#Elbow curve- drawing for k=2 to 15
wss <- (nrow(short_matrix)-1)*sum(apply(short_matrix, 2,var))

for (i in 2:15) {
  wss[i] <- sum(kmeans(short_matrix, centers = i)$withinss)
}

plot(1:15, wss, type="b", xlab="Number of clusters", ylab="WSS")

#from plot we find that cluster number k should be 4 (k=4)
cl_model <- kmeans(short_matrix, 4, nstart = 25)

plot(short_matrix,col=(cl_model$cluster +1),  main="k-means result with 4 clusters", pch=1, cex=1, las=1)
points(cl_model$centers, col=12, pch=8, cex=2)
cl_model

#animation of cluster formation
library(cluster)
#clusplot(short_matrix, cl_model$cluster, color = TRUE, shade = TRUE, lines = 0)
library(animation)
kmeans.ani(short_matrix,4)

#relating cluster assignment with individual component in dataset
aggregate(data=smple, director_facebook_likes ~ cl_model$cluster, mean)
aggregate(data=smple, movie_facebook_likes ~ cl_model$cluster, mean)

#to know profit(mean or centroid) values of each cluster
cl_model$centers
