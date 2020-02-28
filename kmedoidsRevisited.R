setwd('/Users/miche/Desktop/Mathematical Data Science/')
library(fpc)
library(mcl)
library(tibble)
library(qdap)
library(factoextra)
library(cluster)
library(devtools)
library(tm)
library(parallel)
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


# Partitioning around medoids with k number of clusters
k <- 10
pamResult <- pam((binary_matrix), k=k, do.swap=FALSE, stand=TRUE, diss=FALSE, metric="manhattan")
# pamResult2 <- clara((binary_matrix), k=k, metric="manhattan", pamLike=FALSE, correct.d=TRUE)
pamResult$clusinfo
# pamResult2$clusinfo

for(i in 1:k) {cat(paste("cluster", i, ":", sep = ""))
  s <- sort(pamResult$medoids[i,], decreasing = T)
  cat(names(s)[1:4], "\n")}

fviz_cluster(pamResult, stand=TRUE, geom="point", pointsize=1, repel=TRUE)
fviz_silhouette(pamResult)

# Finding the optimal number of clusters
fviz_nbclust(binary_matrix, FUNcluster = pam, k.max=50, method="silhouette")
fviz_nbclust(binary_matrix, FUNcluster = pam, k.max=50, method="wss")
fviz_nbclust(binary_matrix, FUNcluster = pam, k.max=10, method="gap_stat", nboot=1) #doesn't work

# Print tweets from cluster i
print(Trump_tweet$text[pamResult$clustering==i])

# Gap stat based on online guy

# library(doParallel)
# library(iterators)
# library(foreach)
# cores <- 8
# system <- Sys.info()['sysname']
# cl <- NULL
# if (system == 'Windows') {
#   cl <- makeCluster(getOption('cl.cores', cores))
#   registerDoParallel(cl)
#   registerDoSEQ()
#   on.exit(stopCluster(cl))
# } else {
#   options('mc.cores' = cores)
#   registerDoParallel(cores)
# }
# 
# CustomPAM <- function(x,k) list(cluster=pam(
#   x, k,
#   diss=FALSE,
#   metric="manhattan",
#   medoids=NULL,
#   stand=FALSE,
#   cluster.only=TRUE,
#   do.swap=TRUE,
#   keep.diss=FALSE,
#   keep.data=FALSE,
#   pamonce=TRUE,
#   trace.lev=0))
# 
# source("clusGapKB.R")
# gap <- clusGap(binary_matrix, FUNcluster = CustomPAM, K.max=10, B=1)
# fviz_gap_stat(gap, linecolor="steelblue")

