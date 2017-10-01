#k-Means Clustering with R. Iris data set

library(cluster)

names(iris)
set.seed(8952) #Seed to guarantee reproducibility

iris_sample <- iris
iris_sample$Species <- NULL #Dont consider the iris classes, just sepal and petal lenght attributes

kmeans.result_k3 <- kmeans(iris_sample, 3) #Three clusters by kmeans
table(iris$Species, kmeans.result_k3$cluster) #two classes have a small overlap: versicolor and virginica

kmeans.result_k3 #K-means clustering with 3 clusters of sizes 62, 38, 50
kmeans.result_k3$cluster
plot.new()
plot(iris_sample[c("Sepal.Length", "Sepal.Width")], col = kmeans.result_k3$cluster)


#k-Medoids Clustering with R: Clustering with pam()
pam.result_k3 <- pam(iris_sample, 3) #Three clusters
table(pam.result_k3$clustering, iris$Species)
layout(matrix(c(1, 2), 1, 2)) #Two graphes per page
plot(pam.result_k3)

pam.result_k2 <- pam(iris_sample, 2) #Two clusters to increase silhouette SC
layout(matrix(c(1, 2), 1, 2))
plot(pam.result_k2)


#k-Medoids Clustering with R: Clustering with pamk()
library(fpc)
pamk.result <- pamk(iris_sample)
pamk.result$nc #Number of clusters
table(pamk.result$pamobject$clustering, iris$Species)

layout(matrix(c(1, 2), 1, 2))
plot(pamk.result$pamobject)


#Hierarchical Clustering
set.seed(2836)
idx <- sample(1:dim(iris)[1], 30) #Take a sample for visualization purposes
iris_sample_h <- iris[idx, ]
iris_sample_h$Species <- NULL #Dont consider the iris classes
hc_result <- hclust(dist(iris_sample_h), method = "ave")
layout(matrix(c(1, 1), 1, 1))
plot(hc_result, labels = iris$Species[idx])
rect.hclust(hc_result, k = 3) #cut the tree into 3 clusters
