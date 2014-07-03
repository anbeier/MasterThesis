iris2 <- iris
iris2$Species <- NULL    ## delete the Species column
kmeans.result <- kmean(iris2, 3)    ## multi-class
(kmeans.result)    ## show result
table(iris$Species, kmeans.result$cluster)    ## compare the result with class labels
# there are 4 dimensions and we only use the first two of them to plot
plot(iris2[c("Sepal.Length", "Sepal.Width")], col = kmeans.result$cluster)  
points(kmeans.result$centers[,c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex=2)

# k-means clustering
df2 <- dfInt
df2$migration_code_change_in_msa <- NULL
rsltKmeans <- kmeans(df2, 9)
(rsltKmeans)
table(dfInt$migration_code_change_in_msa.factor, rsltKmeans$cluster)
plot(df2[c("age", "education.factor")], col = rsltKmeans$cluster)
points(rsltKmeans$centers[, c("age", "education.factor")], col = 1:9, pch = 8, cex = 2)


# hierarchical clustering
# map age range to 19 classes
distances <- dist(dfInt, method = "euclidean")
migrationCluster <- hclust(distances, method = "ward.D") 

# plot the dendrogram
plot(migrationCluster)

# assign points to clusters
migrationGroups <- cutree(migrationCluster, k = 9)

# create a new data set with just the points from cluster 1
migrationGroup1 <- subset(dfInt, migrationGroups == 1)