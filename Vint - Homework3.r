#CS688
#HW3- Text Mining
library(tm)

#First category training data
atheist.train.loc <- file.path(getwd(),"tm","test","20news-bydate-train","alt.atheism")
atheistTraining <- normalizePath(list.files(path=atheist.train.loc,
                                            pattern = "",
                                            full.names=TRUE))
atheistTrainSubset <- head(atheistTraining,n=100)

#First category test data
atheist.test.loc <- file.path(getwd(),"tm","test","20news-bydate-test","alt.atheism")
atheistTesting <- normalizePath(list.files(path=atheist.test.loc,
                                            pattern = "",
                                            full.names=TRUE))
atheistTestSubset <- head(atheistTesting,n=100)

#Second category training data
space.train.loc <- file.path(getwd(),"tm","test","20news-bydate-train","sci.space")
spaceTraining <- normalizePath(list.files(path=space.train.loc,
                                            pattern = "",
                                            full.names=TRUE))
spaceTrainSubset <- head(spaceTraining,n=100)

#Second category test data
space.test.loc <- file.path(getwd(),"tm","test","20news-bydate-test","sci.space")
spaceTest <- normalizePath(list.files(path=space.test.loc,
                                            pattern = "",
                                            full.names=TRUE))
spaceTestSubset <- head(spaceTest,n=100)

#Obtain the merged corpus
Docs <- URISource(c(atheistTrainSubset,atheistTestSubset,spaceTrainSubset,spaceTestSubset))

Docs.corpus <- VCorpus(Docs)
inspect(Docs.corpus)

#Corpus preprocessing
#to lower case
docs.transf <- tm_map(Docs.corpus,content_transformer(tolower))

#remove punctuation
docs.transf <- tm_map(docs.transf,content_transformer(removePunctuation))

#remove stopwords
docs.transf <- tm_map(docs.transf,removeWords,stopwords("english"))

#stem the docs
#install.packages("SnowballC")
library(SnowballC)
docs.transf <- tm_map(docs.transf,stemDocument)

#Create the document term matrix
dtm <- DocumentTermMatrix(docs.transf,control=list(minWordLength=2,
                                                minDocFreq=5))
#Test out the dtm with a word cloud
#install.packages("wordcloud")
library(wordcloud)

#install.packages("RColorBrewer")
library(RColorBrewer)

freq <- colSums(as.matrix(dtm))
set.seed(123)
wordcloud(names(freq),freq,min.freq=10,colors=brewer.pal(6,"Dark2"))

#split dtm into train and test data
dtm <- as.matrix(dtm)
dtm.train <- dtm[c(1:100,201:300),]
dtm.test <- dtm[c(101:200,301:400),]

#perform knn algorithm
tags <- factor(c(rep("ath",100),
                 rep("spa",100)))

library(class)
prob.test <- knn(dtm.train,dtm.test,tags,k=5,prob=TRUE)
prob.test

#display knn results as daa frame
a <- 1:length(prob.test)
b <- levels(prob.test)[prob.test]
c <- attributes(prob.test)$prob
result <- data.frame(Doc=a,Predict=b,Prob=c,Correct=rep(TRUE,length(prob.test)))

for (i in 1:100) {
  if (result$Predict[i] == "spa") {
    result$Correct[i] = FALSE
  }
}

for (i in 101:200) {
  if (result$Predict[i] == "ath") {
    result$Correct[i] = FALSE
  }
}

result

#Determine knn accuracy
accuracy <- nrow(result[which(result$Correct==TRUE),])/nrow(result)
accuracy
