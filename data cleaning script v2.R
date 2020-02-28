#install.packages("fpc")
#install.packages("mcl")
#install.packages("tibble")
#install.packages("qdap")
#install.packages("devtools")
#install.packages("tm")
#install.packages("stringr")
#install.packages("SnowballC")
#install.packages("cluster")
#install.packages("proxy")
#install.packages("dbscan")
#install.packages("NbClust")
#install.packages("factoextra")
library(fpc)
library(mcl)
library(tibble)
library(qdap)
library(devtools)
library(tm)
library(stringr)
library(dplyr)
library(SnowballC)
library(cluster)
library(proxy)
library(dbscan)
library(NbClust)
library(factoextra)
stopwordslist <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", 
                   "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her",
                   "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", 
                   "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", 
                   "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", 
                   "having", "do", "does", "did", "doing", "a", "an", "the", "and", "but", "if", 
                   "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", 
                   "about", "against", "between", "into", "through", "during", "before", "after",
                   "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", 
                   "under", "again", "further", "then", "once", "here", "there", "when", "where", 
                   "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", 
                   "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", 
                   "very", "s", "t", "can", "will", "just", "don", "should", "now", "way", "didnt", 
                   "hadnt", "said", "cant", "even", "make", "other", "think", "two", "wouldnt", 
                   "along", "made", "come", "let", "dont", "though", "want", "ive", "shouldnt", 
                   "wont", "could", "will", "whose", "couldnt", "there", "doesnt", "what", "went", 
                   "hasnt", "seem", "where", "theyll", "amp", "get", "much", "done", "would", 
                   "must", "come", "make", "got", "isnt", "havent", "think", "thank", "let", "like", 
                   "look", "today", "great", "way", "didnt", "hadnt", "said", "cant", "even", "make", 
                   "other", "think", "two", "wouldnt", "along", "made", "come", "let", "dont", 
                   "though", "want", "ive", "shouldnt", "wont", "could", "will", "whose", "couldnt", 
                   "there", "doesnt", "what", "went", "hasnt", "seem", "where", "theyll", "amp", 
                   "get", "much", "done", "would", "must", "come", "make", "got", "isnt", "havent")
Trump_tweet <- read.csv("Trump_data.csv", header = TRUE, encoding = "UTF-8")
colnames(Trump_tweet) <- c("text")
# remove URL links from the tweets
Trump_tweet$cleaned_text <- gsub("http.*","", Trump_tweet$text)
Trump_tweet$cleaned_text <- gsub("https.*","", Trump_tweet$cleaned_text)
Trump_tweet$cleaned_text <- Trump_tweet %>%
  dplyr::select(cleaned_text)
# extract hashtag
Trump_tweet$hashtag <- sapply(Trump_tweet$text, 
                              function(x) str_extract_all(x, "#\\S+"))
Trump_tweet$hashtag <- vapply(Trump_tweet$hashtag, paste, collapse = ", ", character(1L))
# extract tagged accounts
Trump_tweet$tagged_account <- sapply(Trump_tweet$text, 
                                     function(x) str_extract_all(x, "@\\S+"))
Trump_tweet$tagged_account <- vapply(Trump_tweet$tagged_account, paste, 
                                     collapse = ", ", character(1L))
# extract words with uppercase
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, 
                                   function(x) gsub('#\\S+', '', x)) #remove hashtag
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, 
                                   function(x) gsub('@\\S+', '', x)) #remove mention
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, 
                                   function(x) gsub('[[:punct:]]', '', x)) #remove punctuation
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, 
                                   function(x) gsub('RT', '', x)) #remove RT
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, 
                                   function(x) gsub("[^[:alpha:][:space:]]*", "", x)) #remove anything other than English words and space
Trump_tweet$filtered <- sapply(Trump_tweet$cleaned_text, 
                               function(x) tolower(x)) # convert all the uppercase to lowercase
Trump_tweet$filtered <- sapply(Trump_tweet$filtered, 
                               function(x) removeWords(x, stopwordslist)) # remove stop words
Trump_tweet$filtered <- sapply(Trump_tweet$filtered, 
                               function(x) gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)) # remove extra space
Trump_tweet <- Trump_tweet[! Trump_tweet$filtered == "",]
# create a corpus containing all words from the cleaned text
corpus <- Corpus(VectorSource(Trump_tweet$filtered))
# stemming
corpus <- tm_map(corpus, stemDocument)
# remove words
corpus <- tm_map(corpus, removeWords, stopwordslist)
# create a matrix with unique words
DTM <- DocumentTermMatrix(corpus)
# remove the less common words
sparse_DTM <- removeSparseTerms(DTM, 0.995)
# construct binary matrix
binary_matrix <- as.matrix(sparse_DTM)
binary_matrix[binary_matrix > 0] <- 1


### K-MEANS
# determine k value
kmean_nbclust <- NbClust(data = binary_matrix, diss = NULL, distance = "manhattan",
                         min.nc = 2, max.nc = 20, method = "kmeans")
# choose k = 13
clustering_kmeans <- kmeans(binary_matrix, centers = 13, iter.max = 100, nstart = 20, set.seed(1))

# print the top three words in every cluster
for(i in 1:13) {cat(paste("cluster", i, ":", sep = ""))
  s <- sort(clustering_kmeans$center[i,], decreasing = T)
  cat(names(s)[1:5], "\n")}

# plot
kmeans_res <- eclust(binary_matrix, "kmeans", k = 13, nstart = 25, graph = FALSE)
fviz_cluster(kmeans_res, geom = "point", ellipse.type = "norm", ggtheme = theme_minimal())

### K-MEDOIDS
# determine k value
fviz_nbclust(binary_matrix, pam, method = "silhouette", k.max = 50) + theme_classic()
fviz_nbclust(binary_matrix, FUNcluster = pam, method = "wss", k.max = 50) + theme_classic()

# choose k = 7
clustering_kmedoids <- pam(binary_matrix, 7)

# print the top three words in every cluster
for(i in 1:7) {cat(paste("cluster", i, ":", sep = ""))
  s <- sort(clustering_kmedoids$medoids[i,], decreasing = T)
  cat(names(s)[1:3], "\n")}

# plot
fviz_cluster(clustering_kmedoids, binary_matrix, stand = TRUE, 
             geom = "point", repel = TRUE, ggtheme = theme_classic())


### HIERARCHICAL 
# determine number of cluster
hierarchical_nbclust <- NbClust(binary_matrix, distance = "manhattan", min.nc = 2,   
              max.nc = 50, method = "ward.D2")

# choose k = 
# plot
hierarchical_res <- agnes(x = binary_matrix, stand = TRUE, metric = "manhattan", method = "ward")
fviz_dend(hierarchical_res, cex = 0.6, k = 4, rect = TRUE)
fviz_dend(hierarchical_res, k = 4, k_colors = "jco", 
          type = "phylogenic", relep = TRUE , phylo_layout = "layout_as_tree")
fviz_dend(hierarchical_res, cex = 0.6, k = 4, type = "circular", rect = TRUE)


### DBSCAN
# determine the value of k
k = dim(binary_matrix)[2]*2

#make  distance matrix!!
dis_matrix <- dist(binary_matrix, method = "manhattan")

#plot the k least distances sorted by distance
kNNdistplot(dis_matrix, k = k )

grid(nx = 2, ny = 8, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

# choose eps = 22
# compute DBSCAN
set.seed(123)
dbscan_res <- dbscan(dis_matrix, eps = 22, minPts = k)

# plot DBSCAN result
fviz_cluster(dbscan_res, data = dis_matrix, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())

# Determine k value (rule of thump: k = 2*dim)
k = dim(binary_matrix)[2]*2

# Create MANHATTAN distance matrix
manh_mat_dist <-  as.matrix(dist(binary_matrix, method = "manhattan"))

# Plot the k least distances sorted by distance
kNNdistplot(manh_mat_dist, k = k )

grid(nx = 2, ny = 8, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


# Choose eps = 200 (where ther is a sharp increase in distance)
# Compute DBSCAN
set.seed(123)
dbscan_res <- dbscan(manh_mat_dist, eps = 200, minPts = k)

# plot DBSCAN result
fviz_cluster(dbscan_res, data = manh_mat_dist, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",ggtheme = theme_classic())

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

# Create MANHATTAN distance matrix
distMatrix <- dist(binary_matrix, method = "manhattan")

# Compute hierarchical clustering
fit <- hclust(distMatrix, method="ward.D2")

# Choose k = 12
k = 12


# Print the to 5 words clusters
for(i in 1:k) {cat(paste("cluster", i, ":", sep = ""))
  #fit$cluster[fitk[[i]]] <- i
  s <- binary_matrix[fitk[[i]], order(colSums(binary_matrix[fitk[[i]],]), decreasing = TRUE)]
  cat(colnames(s)[1:5], "\n")}

# Plot 
fviz_dend(fit, cex = 0.6, k = k, rect = TRUE)
