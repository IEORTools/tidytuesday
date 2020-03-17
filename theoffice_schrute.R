library(tidyverse)
library(tm)

install.packages("schrute")
library(schrute)

mydata <- schrute::theoffice

michaeltext <- mydata$text[which(mydata$character=="Michael")]


#### word cloud

mtext <- paste(michaeltext, collapse = " ")

vs <- VectorSource(mtext)
michaelcorpus <- VCorpus(vs)
michaelcorpus <- tm_map(michaelcorpus, removePunctuation)
michaelcorpus <- tm_map(michaelcorpus, removeNumbers)
michaelcorpus <- tm_map(michaelcorpus, function(x)removeWords(x,stopwords()))
michaelcorpus <- tm_map(michaelcorpus, function(x)removeSparseTerms(x, 0.1))


term.matrix <- TermDocumentMatrix(michaelcorpus)
term.matrix <- as.matrix(term.matrix)

commonality.cloud(term.matrix,max.words=100,random.order=FALSE)

