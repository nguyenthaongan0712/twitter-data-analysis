#rule of thump: k = 2*dim
k = dim(binary_matrix)[2]*2

#make BINARY distance matrix!!
bin_mat_dist <- dist(binary_matrix, method = "binary")

#plot the k least distances sorted by distance
dbscan::kNNdistplot(bin_mat_dist, k = k )

grid(nx = 2, ny = 8, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


#test different epsilons
eps <- 0.7
dbscan_db <- dbscan::dbscan(bin_mat_dist, eps = eps, minPts = k)
print(dbscan_db)


#runs for several epsilons
eps_start<- 0.85
diff <- 0.15
n <-10
for (i in 0:n){
  eps <- eps_start + (i/n)*diff
  dbscan_db <- dbscan::dbscan(bin_mat_dist, eps = eps, minPts = k)
  print(dbscan_db)
}










#make MANHATTAN distance matrix!!
manh_mat_dist <-  dist(binary_matrix, method = "manhattan")

#plot the k least distances sorted by distance
dbscan::kNNdistplot(manh_mat_dist, k = k )

grid(nx = 2, ny = 8, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


#test different epsilons
eps<- 1
dbscan_db <- dbscan::dbscan(manh_mat_dist, eps = eps, minPts = k)
print(dbscan_db)


#runs for several epsilons
eps_start<- 1
diff <- 14
n <-10
for (i in 0:n){
  eps <- eps_start + (i/n)*diff
  dbscan_db <- dbscan::dbscan(manh_mat_dist, eps = eps, minPts = k)
  print(dbscan_db)
}
