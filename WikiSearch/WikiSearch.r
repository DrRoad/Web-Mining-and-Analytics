# Example: Shiny app that search Wikipedia web pages
# File: WikiSearch.R
library(tm)
library(stringi)
library(proxy)
library(XML)
SearchWiki <- function (titles) {
  wiki.URL <- "http://en.wikipedia.org/wiki/"
  articles <- lapply(titles,function(i) stri_flatten(readLines(stri_paste(wiki.URL,i)), col = " "))
  docs <- Corpus(VectorSource(articles)) # Get Web Pages' Corpus
  remove(articles)
  # Text analysis - Preprocessing 
  transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
  temp <- tm_map(docs, transform.words, "<.+?>", " ")
  temp <- tm_map(temp, transform.words, "\t", " ")
  temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, stripWhitespace)
  temp <- tm_map(temp, removeWords, stopwords("english"))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
  remove(docs)
  # Create Dtm 
  dtm <- DocumentTermMatrix(temp) # Document term matrix
  dtm <- removeSparseTerms(dtm, 0.4) # Reduce Document term matrix
  docsdissim <- dist(as.matrix(dtm), method = "cosine") # Distance Measure
  set.seed(0)  
  h <- hclust(as.dist(docsdissim), method = "ward.D2") # Group Results
}

SearchCloud <- function (titles) {
  wiki.URL <- "http://en.wikipedia.org/wiki/"
  articles <- lapply(titles,function(i) stri_flatten(readLines(stri_paste(wiki.URL,i)), col = " "))
  docs <- Corpus(VectorSource(articles)) # Get Web Pages' Corpus
  remove(articles)
  # Text analysis - Preprocessing 
  transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
  temp <- tm_map(docs, transform.words, "<.+?>", " ")
  temp <- tm_map(temp, transform.words, "\t", " ")
  temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, stripWhitespace)
  temp <- tm_map(temp, removeNumbers)
  temp <- tm_map(temp, removeWords, stopwords("english"))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
  remove(docs)
  # Create Dtm 
  dtm <- DocumentTermMatrix(temp) # Document term matrix
  dtm <- removeSparseTerms(dtm, 0.4) # Reduce Document term matrix
  freqTerms <- colSums(as.matrix(dtm))
  wc <- wordcloud(names(freqTerms),freqTerms,min.freq=10,colors=rainbow(6))
  return(wc)
}

FrequentFive <- function (titles) {
  wiki.URL <- "http://en.wikipedia.org/wiki/"
  articles <- lapply(titles,function(i) stri_flatten(readLines(stri_paste(wiki.URL,i)), col = " "))
  docs <- Corpus(VectorSource(articles)) # Get Web Pages' Corpus
  remove(articles)
  # Text analysis - Preprocessing 
  transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
  temp <- tm_map(docs, transform.words, "<ââà.+?>", " ")
  temp <- tm_map(temp, transform.words, "\t", " ")
  temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, stripWhitespace)
  temp <- tm_map(temp, removeWords, stopwords("english"))
  temp <- tm_map(temp, removeNumbers)
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
  remove(docs)
  # Create Dtm 
  dtm <- DocumentTermMatrix(temp) # Document term matrix
  dtm <- removeSparseTerms(dtm, 0.4) # Reduce Document term matrix
  freqTerms <- colSums(as.matrix(dtm))
  return(freqTerms)
}