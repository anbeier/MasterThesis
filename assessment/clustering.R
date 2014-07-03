iris2 <- iris
iris2$Species <- NULL    ## delete the Species column
## k-means clustering
qscl <- dfInt
qscl$migration_code_change_in_msa.factor <- NULL    ## remove the target variable
resKmeans <- kmeans(qscl, 9)

# take a look at the clusters
(resKmeans)

# compare clustering results with class labels
table(resKmeans$cluster, dfInt$migration_code_change_in_msa.factor)

# plot clusters
plot(qscl[c("weeks_worked_in_year", "education.factor")], col = resKmeans$cluster)
points(resKmeans$centers[, c("weeks_worked_in_year", "education.factor")], col = 1:9, pch = 20, cex = 2)


## density-based clustering
install.packages("fpc")
library(fpc)
qscl <- dfInt
qscl$migration_code_change_in_msa.factor <- NULL
ds <- dbscan(qscl, eps=0.42, MinPts=5)
table(ds$cluster, dfInt$migration_code_change_in_msa.factor)

plot(ds, qscl)


## hierarchical clustering
# map age range to 19 classes
distances <- dist(dfInt, method = "euclidean")
migrationCluster <- hclust(distances, method = "ward.D") 

# plot the dendrogram
plot(migrationCluster)

# assign points to clusters
migrationGroups <- cutree(migrationCluster, k = 9)

# create a new data set with just the points from cluster 1
migrationGroup1 <- subset(dfInt, migrationGroups == 1)