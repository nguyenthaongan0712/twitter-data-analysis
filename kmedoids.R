setwd('/Users/miche/Desktop/Mathematical Data Science/')
#install.packages("SnowballC")
#install.packages("stringr")
#install.packages("tm")
#install.packages("dplyr")
# install.packages("qdap")
# install.packages("fpc")
library(fpc)
library(mcl)
library(tibble)
library(qdap)
library(cluster)
library(devtools)
library(tm)
library(stringr)
library(dplyr)
library(SnowballC)
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
                   "look", "today", "great")
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


# partitioning around medoids with k number of clusters
k <- 10
pamResult <- pam((binary_matrix), k=k, do.swap=FALSE, stand=TRUE, diss=FALSE, metric="euclidean")

rdmTweets <- Trump_tweet$filtered
# print cluster medoids
for (i in 1:k) {
  cat(paste("cluster", i, ": "))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")}
#  print tweets in cluster i
#  print(rdmTweets[pamResult$clustering==i])


# plot clustering result
layout(matrix(c(1,2),2,1)) # set to two graphs per page
plot(pamResult, color=F, labels=k, lines=0, cex=.8, col.clus=1, col.p=pamResult$clustering)
layout(matrix(1)) # change back to one graph per page