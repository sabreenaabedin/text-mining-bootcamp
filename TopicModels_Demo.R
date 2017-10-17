

######Preparing Data

library(rvest)
JJ_site <- html("https://www.cleanandclear.com/facial-cleansers/morning-burst-facial-cleanser#bv-product-reviews")

setwd("C:/Users/Student/Documents/Capstone")

#send text output to a text file
sink("outfile.txt")

#scrape html code for a specific tag
txt <- JJ_site %>%
  html_nodes(".field--name-text .even") %>%
  html_text() 

#write to file
cat(txt)
sink()

aFile = readLines("outfile.txt")

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
wordcloud(names(v), v, min.freq = 3)

myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))

######### Topic Modeling

library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(topicmodels)

n_topics <- 6
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
