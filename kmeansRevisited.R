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
#install.packages("qdap")
library(qdap)
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
library(dplyr)
stopwordslist <- c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", 
                   "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", 
                   "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", 
                   "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", 
                   "does", "did", "doing", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", 
                   "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", 
                   "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", 
                   "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", 
                   "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", 
                   "s", "t", "can", "will", "just", "don", "should", "now", "way", "didnt", "hadnt", "said", "cant", "even", "make", 
                   "other", "think", "two", "wouldnt", "along", "made", "come", "let", "dont", "though", "want", "ive", "shouldnt", 
                   "wont", "could", "will", "whose", "couldnt", "there", "doesnt", "what", "went", "hasnt", "seem", "where", "theyll", 
                   "amp", "get", "much", "done", "would", "must", "come", "make", "got", "isnt", "havent", "think", "thank", "let", "like", 
                   "look", "today", "great", "way", "didnt", "hadnt", "said", "cant", "even", "make", "other", "think", "two", 
                   "wouldnt", "along", "made", "come", "let", "dont", "though", "want", "ive", "shouldnt", "wont", "could", "will", 
                   "whose", "couldnt", "there", "doesnt", "what", "went", "hasnt", "seem", "where", "theyll", "amp", "get", "much", 
                   "done", "would", "must", "come", "make", "got", "isnt", "havent")
Trump_tweet <- read.csv("Trump_data.csv", header = TRUE, encoding = "UTF-8")
colnames(Trump_tweet) <- c("text")
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



# computing the optimal value of k with elbow plot
set.seed(1)

# compute and plot wss for k = 2 to k = 50
k.max <- 50

wss <- sapply(2:k.max, function(k){kmeans(binary_matrix, k, nstart = 25, iter.max = 15)$tot.withinss})
wss
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



# Optimal value of k with NbClust

 res <- NbClust(binary_matrix, diss = NULL, distance = "manhattan", min.nc = 2, max.nc = 50, method = "kmeans", index = "all", alphaBeale = 0.1)

 res$All.index
 res$All.CriticalValues
 res$Best.nc
 res$Best.partition


#k-means for k = 13
k = 13
kmeans.result <- kmeans(binary_matrix, centers = k, iter.max = 100, nstart = 20)

#cluster centers
round(kmeans.result$centers, digits = 3)

#cluster IDs
kmeans.result$cluster

#cluster sizes
kmeans.result$size

#print the top five words in every cluster
for(i in 1:k) {cat(paste("cluster", i, ":", sep = ""))
  s <- sort(kmeans.result$center[i,], decreasing = T)
  cat(names(s)[1:5], "\n")}

