setwd("C:/Users/Mikkel/Documents/R/Files")
#install.packages("stringr")
#install.packages("tm")
#install.packages("tidyverse")
#install.packages("SnowballC")
#install.packages("cluster")
#install.packages(c("factoextra", "fpc", "NbClust"))
#install.packages("clValid")
#install.packages("mclust")
#install.packages("NbClust", dependecies = TRUE)
library(mclust)
library(clValid)
library(factoextra)
library(fpc)
library(NbClust)
library(cluster)
library(SnowballC)
library(tm)
library(stringr)
library(tidyverse)
stopwordslist <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", 
                   "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", 
                   "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", 
                   "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", 
                   "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", 
                   "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", 
                   "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", 
                   "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", 
                   "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", 
                   "s", "t", "can", "will", "just", "don", "should", "now", "way", "didnt", "hadnt", "said", "cant", "even",
                   "make", "other", "think", "two", "wouldnt", "along", "made", "come", "let", "dont", "though", "want", "that",
                   "ive", "shouldnt", "wont", "could", "will", "whose", "couldnt", "there", "doesnt", "what", "went", "hasnt",
                   "seem", "where", "theyll")
Trump_tweet <- read.csv("Trump_data.csv", header = TRUE)
# remove URL links from the tweets
Trump_tweet$cleaned_text <- gsub("http.*","", Trump_tweet$text)
Trump_tweet$cleaned_text <- gsub("https.*","", Trump_tweet$cleaned_text)
Trump_tweet$cleaned_text <- Trump_tweet %>%
  dplyr::select(cleaned_text)
# extract hashtag
Trump_tweet$hashtag <- sapply(Trump_tweet$text, function(x) str_extract_all(x, "#\\S+"))
Trump_tweet$hashtag <- vapply(Trump_tweet$hashtag, paste, collapse = ", ", character(1L))
# extract tagged accounts
Trump_tweet$tagged_account <- sapply(Trump_tweet$text, function(x) str_extract_all(x, "@\\S+"))
Trump_tweet$tagged_account <- vapply(Trump_tweet$tagged_account, paste, collapse = ", ", character(1L))
# extract words with uppercase
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, function(x) gsub('#\\S+', '', x)) #remove hashtag
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, function(x) gsub('@\\S+', '', x)) #remove mention
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, function(x) gsub('[[:punct:]]', '', x)) #remove punctuation
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, function(x) gsub('RT', '', x)) #remove RT
Trump_tweet$cleaned_text <- sapply(Trump_tweet$cleaned_text, function(x) gsub("[^[:alpha:][:space:]]*", "", x)) #remove anything other than English words and space
Trump_tweet$filtered <- sapply(Trump_tweet$cleaned_text, function(x) tolower(x)) # convert all the uppercase to lowercase
Trump_tweet$filtered <- sapply(Trump_tweet$filtered, function(x) removeWords(x, stopwordslist)) # remove stop words
Trump_tweet$filtered <- sapply(Trump_tweet$filtered, function(x) gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)) # remove extra space
# create a corpus containing all words from the cleaned text
corpus <- Corpus(VectorSource(Trump_tweet$filtered))
# stemming
corpus <- tm_map(corpus, stemDocument)
# create a matrix with unique words
DTM <- DocumentTermMatrix(corpus)
# remove the less common words
sparse_DTM <- removeSparseTerms(DTM, 0.999)
# construct binary matrix
binary_matrix <- as.matrix(sparse_DTM)
binary_matrix[binary_matrix > 0] <- 1

#frequent_terms <- freq_terms(Trump_tweet$filtered, 30)
#plot(frequent_terms)





# Trying out kmeans for k = 20
k = 20
kmeans.result <- kmeans(binary_matrix, centers = k, iter.max = 100, nstart = 20, set.seed(1))

#cluster centers
round(kmeans.result$centers, digits = 3)

#cluster IDs
kmeans.result$cluster

#print the top three words in every cluster
for(i in 1:k) {cat(paste("cluster", i, ":", sep = ""))
  s <- sort(kmeans.result$center[i,], decreasing = T)
  cat(names(s)[1:5], "\n")}


# #distances between objects and cluster centers
##centers <- kmeans.result$centers[kmeans.result$cluster, ]
##distances <- sqrt(rowSums((binary_matrix - centers)^2))
# # pick top 20 largest distances
##outliers <- order(distances, decreasing=T)[1:20]
# # who are outliers
##print(outliers)







# # Determining optimal k using Bayesian Information Criterion (gives an error)

# d_clust <- Mclust(binary_matrix, G = 10:20, modelnames = mclust.options("emModelnames"))
# d_clust$BIC
# plot(d_clust)






# # Determining optimal k using NbClust
# nb <- NbClust(binary_matrix, diss=NULL, distance = "binary", 
#               min.nc=20, max.nc=100, method = "kmeans", 
#               index = "dunn", alphaBeale = 0.1)
# hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))









# # computing the optimal value of k (no valuable information)
set.seed(123)
# 
# # compute and plot wss for k = 5 to k = 100
k.max <- 100
# 
wss <- sapply(5:k.max, function(k){kmeans(binary_matrix, k, nstart = 25, iter.max = 15)$tot.withinss})
wss
plot(5:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
