distMatrix <- dist(binary_matrix, method = "euclidean") 
# Choice of distance method is still unsure (method = "binary" produces too `simple` clusters)
fit <- hclust(distMatrix, method="ward.D")

plot(fit, labels = FALSE)

k=10 # Number of clusters
fitk <- rect.hclust(fit, k)
for(i in 1:k) {cat(paste("cluster", i, ":", sep = ""))
  #fit$cluster[fitk[[i]]] <- i
  s <- binary_matrix[fitk[[i]], order(colSums(binary_matrix[fitk[[i]],]), decreasing = TRUE)]
  cat(colnames(s)[1:5], "\n")}


# Sampling random tweets inside a cluster (c < k)
c <- 4 # Cluster number to sample from
n <- 3 # Number of tweets to sample
row_samp <- sample(nrow(Trump_data[fitk[[c]],]), n)
clus_samp <- fitk[[c]][row_samp]
Trump_data[clus_samp,]