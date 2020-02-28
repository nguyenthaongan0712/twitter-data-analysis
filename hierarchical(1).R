distMatrix <- dist(binary_matrix, method = "manhattan")
fit <- hclust(distMatrix, method="ward.D2")

plot(fit, labels = FALSE)

k=12 # Number of clusters
fitk <- rect.hclust(fit, k, border = "red2")
for(i in 1:k) {cat(paste("cluster", i, ":", sep = ""))
  #fit$cluster[fitk[[i]]] <- i
  s <- binary_matrix[fitk[[i]], order(colSums(binary_matrix[fitk[[i]],]), decreasing = TRUE)]
  cat(colnames(s)[1:8], "\n")}


# Sampling random tweets inside a cluster (c < k)
c <- 4 # Cluster number to sample from
n <- 3 # Number of tweets to sample
row_samp <- sample(nrow(Trump_data[fitk[[c]],]), n)
clus_samp <- fitk[[c]][row_samp]
Trump_data[clus_samp,]