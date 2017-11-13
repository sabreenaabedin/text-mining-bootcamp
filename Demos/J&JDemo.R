install.packages("rvest")
library(rvest)
JJ_site <- html("https://www.cleanandclear.com/facial-cleansers/morning-burst-facial-cleanser#bv-product-reviews")

setwd("/Users/timsander/Documents/Capstone")

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



#simple sentence classification as pos/neg using Naive Bayes
#code: http://www.cs.cornell.edu/people/pabo/movie-review-data/
#tutorial: http://andybromberg.com/sentiment-analysis/

#import libraries to work with
library(plyr)
library(stringr)
library(e1071)    

#load up word polarity list and format it
afinn_list <- read.delim(file='AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)    

#categorize words as very negative to very positive and add some movie-specific words
#we would put in words specific to J&J products
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")    

#load up positive and negative sentences and format
posText <- read.delim(file='movies/movies/data/pos.txt', header=FALSE, stringsAsFactors=FALSE)
posText <- posText$V1
posText <- unlist(lapply(posText, function(x) { str_split(x, "\n") }))
negText <- read.delim(file='movies/movies/data/neg.txt', header=FALSE, stringsAsFactors=FALSE)
negText <- negText$V1
negText <- unlist(lapply(negText, function(x) { str_split(x, "\n") }))    

#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    

#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(posText, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negText, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')    

#combine the positive and negative tables
posResult
negResult

results <- rbind(posResult, negResult)    

results <- results[sample(1:nrow(results)), ]

testSet <- results[1:2500,]
trainSet <- results[2501:10663,]

#run the naive bayes algorithm using all four categories
classifier <- naiveBayes(trainSet[,2:5], trainSet[,6])    

#display the confusion table for the classification ran on the same data
confTable <- table(predict(classifier, testSet), testSet[,6], dnn=list('predicted','actual'))
confTable    

#run a binomial test for confidence interval of results
binom.test(confTable[1,1] + confTable[2,2], nrow(testSet), p=0.5)



