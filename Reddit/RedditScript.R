######Preparing Data

library(rvest)

setwd("/Users/timsander/Documents/Capstone")

aFile = readLines("lemonComments.txt")


library(tm)
myCorpus = Corpus(VectorSource(aFile))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)

library(wordcloud)
set.seed(4363)
par(mar=c(1,1,1,1))
wordcloud(names(v), v, min.freq = 50)

myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))

rowTotals <- apply(myDTM , 1, sum) #Find the sum of words in each Document
dtm.new   <- myDTM[rowTotals> 0, ]           #remove all docs without words

myDTM <- dtm.new

######### Topic Modeling

#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("stringr")
#install.packages("tm")
#install.packages("topicmodels")
#install.packages("ggplot2")

library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(topicmodels)
library(dplyr)
library(ggplot2)

n_topics <- 6

myDTM

myDTM.lda <- LDA(myDTM, k = n_topics, control = list(seed = 1234))

myDTM.lda

myDTM.topics <- tidy(myDTM.lda, matrix = "beta") 

myDTM.top_terms <- myDTM.topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, term, -beta)

myDTM.top_terms2 <- myDTM.top_terms %>%
  group_by(topic) %>%
  summarise(terms = toString(sort(unique(term))))
myDTM.top_terms2

myDTM.top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

##Adding Stop Words


myCorpus <- tm_map(myCorpus, removeWords, c("face","skin"))
myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))

rowTotals <- apply(myDTM , 1, sum) #Find the sum of words in each Document
dtm.new   <- myDTM[rowTotals> 0, ]           #remove all docs without words

myDTM <- dtm.new


myDTM.lda <- LDA(myDTM, k = n_topics, control = list(seed = 1234))

myDTM.top_terms <- myDTM.topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, term, -beta)

myDTM.top_terms2 <- myDTM.top_terms %>%
  group_by(topic) %>%
  summarise(terms = toString(sort(unique(term))))
myDTM.top_terms2

myDTM.top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
