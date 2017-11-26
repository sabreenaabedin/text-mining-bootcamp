library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)
library(genderizeR)
library(gender)
library(tidyverse)
library(tidytext)
library(stringr)
library(topicmodels)
library(reshape2)

##### LOAD TEXTS #####


setwd("~/SYS4021")
source("AccidentInput.R")

acts <- file.inputl("~/Capstone/Facebook") 
comvar <- intersect(colnames(acts[[1]]), colnames(acts[[2]]))
totacts <- combine.data(acts, comvar)

## Find gender ##

totacts$comment_author <- as.character(totacts$comment_author)
totacts$name <- sapply(strsplit(totacts$comment_author,' '), function(x) x[1])
name <- gender(as.character(totacts$name), years = c(1932,2012))[1:4]

# Get an inner join 
df <- inner_join(totacts, name, by = "name")

# Remove duplicated data
df <- df[!duplicated(df),]

# Take out first names
df <- df[, -c(13:15)]

# Change to a tibble
df <- as.tibble(df)

# Split up into male and female dataframes

dfm <- df[df$gender == "male",]
dff <- df[df$gender == "female",]

##### BEGIN CLEANING #####

## First Males##

# Get cleaned raw text 
some_txtm <- as.character(dfm$comment_message)
some_txtm = gsub("[[:punct:]]", "", some_txtm)
some_txtm = gsub("[[:digit:]]", "", some_txtm)
some_txtm = gsub("http\\w+", "", some_txtm)
some_txtm = gsub("[ \t]{2,}", "", some_txtm)
some_txtm = gsub("^\\s+|\\s+$", "", some_txtm)
some_txtm = gsub("[^\x01-\x7F]+","",some_txtm)
# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

some_txtm = sapply(some_txtm, try.error)
some_txtm = some_txtm[!is.na(some_txtm)]
names(some_txtm) = NULL
dfm$comment_message <- some_txtm

# Get corpus
doxm <- dfm$comment_message
doxm <- Corpus(VectorSource(doxm)) 
doxm <- tm_map(doxm,content_transformer(tolower)) 
doxm <- tm_map(doxm,stripWhitespace) 
doxm <- tm_map(doxm,removeWords,stopwords('english'))  #FREEZING ON THIS ONE
doxm <- tm_map(doxm,removePunctuation)
doxm <- tm_map(doxm,stemDocument)
doxm <- tm_map(doxm, removeNumbers)

# Get term document matrix and document term matrix 
dtmm <- DocumentTermMatrix(doxm)
tdmm <- TermDocumentMatrix(doxm)

## Next females##

# Get cleaned raw text 
some_txtf <- as.character(dff$comment_message)
some_txtf = gsub("[[:punct:]]", "", some_txtf)
some_txtf = gsub("[[:digit:]]", "", some_txtf)
some_txtf = gsub("http\\w+", "", some_txtf)
some_txtf = gsub("[ \t]{2,}", "", some_txtf)
some_txtf = gsub("^\\s+|\\s+$", "", some_txtf)
some_txtf = gsub("[^\x01-\x7F]+","",some_txtf)
some_txtf = sapply(some_txtf, try.error)
some_txtf = some_txtf[!is.na(some_txtf)]
names(some_txtf) = NULL
dff$comment_message <- some_txtf

# Get corpus
doxf <- dff$comment_message
doxf <- Corpus(VectorSource(doxf)) 
doxf <- tm_map(doxf,content_transformer(tolower)) 
doxf <- tm_map(doxf,stripWhitespace) 
doxf <- tm_map(doxf,removeWords,stopwords('english')) 
doxf <- tm_map(doxf,removePunctuation)
doxf <- tm_map(doxf,stemDocument)
doxf <- tm_map(doxf, removeNumbers)
doxf <- tm_map(doxf, function(x) iconv(enc2utf8(x), sub = "byte"))

# Get term document matrix and documnet term matrix
dtmf <- DocumentTermMatrix(doxf)
tdmf <- TermDocumentMatrix(doxf)


## Next everyone ##

# Get cleaned raw text 
some_txt <- as.character(df$comment_message)
some_txt = gsub("[[:punct:]]", "", some_txt)
some_txt = gsub("[[:digit:]]", "", some_txt)
some_txt = gsub("http\\w+", "", some_txt)
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub("[^\x01-\x7F]+","",some_txt)
some_txt = sapply(some_txt, try.error)
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
df$comment_message <- some_txt

# Get corpus
dox <- df$comment_message
dox <- Corpus(VectorSource(dox)) 
dox <- tm_map(dox,content_transformer(tolower)) 
dox <- tm_map(dox,stripWhitespace) 
dox <- tm_map(dox,removeWords,stopwords('english'))  
dox <- tm_map(dox,removePunctuation)
dox <- tm_map(dox,stemDocument)
dox <- tm_map(dox, removeNumbers)
dox <- tm_map(dox, function(x) iconv(enc2utf8(x), sub = "byte"))

# Get term document matrix and documnet term matrix
dtm <- DocumentTermMatrix(dox)
tdm <- TermDocumentMatrix(dox)



#### BEGIN ANALYSIS ####

## general viz ##

# First un-nest all the words 
unnestdf <- df %>%
  unnest_tokens(word, comment_message) %>%
  anti_join(stop_words)

# Look at sentiment analysis histogram by gender
unnestdf %>%
  inner_join(get_sentiments("afinn")) %>%
  ggplot(aes(score, fill = gender)) +
  geom_histogram(aes(y = ..density..),binwidth = 1, show.legend = FALSE) + 
  facet_wrap(~gender)

# Look at sentiment and gender for top words 
unnestdf %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, gender, sort = TRUE) %>%
  ungroup() %>%
  group_by(sentiment, gender) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment + gender, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# Look at sentiment and word frequency by gender in word cloud format 
unnestdf %>%
  inner_join(get_sentiments("bing")) %>%
  dplyr::count(word, sentiment, gender, sort = TRUE) %>%
  acast(word ~ sentiment + gender, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red", "darkblue", "maroon1", "seagreen1"),
                   max.words = 150)


# Look at most word occurences by gender -- NOT USEFUL!
# unnestdf %>%
#   dplyr::count(word, gender, sort = TRUE) %>%
#   ungroup() %>%
#   group_by(gender) %>%
#   top_n(15) %>%
#   ungroup() %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n, fill = gender)) +
#   geom_col() +
#   facet_wrap(~gender , scales = "free_y") +
#   xlab(NULL) +
#   coord_flip()

## tf - idf ##

tfdf <- unnestdf %>%
  dplyr::count(gender, word, sort = TRUE) %>%
  ungroup()


tfdf <- tfdf %>%
  bind_tf_idf(word,gender,n)

tfdf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(gender) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~gender, scales = "free") +
  coord_flip()


mystopwords <- data_frame(word = c("derm","facerealityacneclinic","facereality","luxxe","thy","pls","wwwseacretdirectcomkimfehr",
                                   "someones"))
  
tfdf <- anti_join(tfdf, mystopwords, by = "word")

# Final graph from tf - idf

tfdf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(gender) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = gender)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~gender, scales = "free") +
  coord_flip()


## bigrams ##

dfbigram <- df %>%
  unnest_tokens(bigram, comment_message, token = "ngrams", n = 2) 

dfbigramsep <- dfbigram %>%
  separate(bigram, c("word1","word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

dfbigramsep %>%
  dplyr::count(word1,word2,sort = TRUE)

dfbigramsep %>%
  filter(word1 == "acne") %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  dplyr::count(word2, score, sort = TRUE) %>%
  ungroup()  %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

## Topic Modeling ##


rowTotals <- apply(dtm , 1, sum) 
dtm.new   <- dtm[rowTotals > 0, ] 

n_topics <- 6
myDTM.lda <- LDA(dtm.new, k = n_topics, control = list(seed = 1234))

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

myCorpus <- tm_map(dox, removeWords, c("face","skin","use","acn","tri","ive","will")) # Get better stop words 
myDTM = DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))

rowTotals <- apply(myDTM , 1, sum) 
dtm.new   <- myDTM[rowTotals> 0, ] 

myDTM.lda <- LDA(dtm.new, k = n_topics, control = list(seed = 1234))

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

# Final graph ! 

myDTM.top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()
