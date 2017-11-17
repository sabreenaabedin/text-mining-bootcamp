
library(twitteR)
library(base64enc)
library(ROAuth)
require(RCurl)
library(stringr)
library(ggmap)
library(dplyr)
library(plyr)
library(tm)
library(wordcloud)

## Now we authenticate

key='vzfbkFKrWI48jfCp8sEsLNv0E'
secret='kBw4o5YZyqe1kvapC4eixurzXN87qVwMG9ORiSsbZ063gJtARx'
setwd("C:/Users/Student/Documents/DS4559/Day7")
access_token='708225866-xxOcvjumf6JNFMbT1BDzGlCuCxWOYcxLdjhBMeP0'
access_token_secret='WqnWo5HKaeIfCmkQ1mkKWO8hAt9JZVVNtf6jFOE5Byoee'

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="C:/Users/Student/Documents/DS4559/cacert.pem",
              method="curl")
authenticate <- OAuthFactory$new(consumerKey=key,
                                 consumerSecret=secret,
                                 requestURL="https://api.twitter.com/oauth/request_token",
                                 accessURL="https://api.twitter.com/oauth/access_token",
                                 authURL="https://api.twitter.com/oauth/authorize")
setup_twitter_oauth(key, secret, access_token, access_token_secret)
save(authenticate, file="twitter authentication.Rdata")

# harvest some tweets
skincare_terms = c("Acne","Pimple","Skincare","Face wash","Clear skin")
some_tweets = searchTwitter(skincare_terms, n=5000, lang="en")

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub('???','',some_txt)
some_txt = gsub('etc','',some_txt)
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
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

#write.csv(some_txt, 'acne_tweets.csv')

col=brewer.pal(6,"Dark2")
wordcloud(some_txt, min.freq=5, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=100, random.order=F,colors=col)

## Here we perform some sentiment analysis:

## First we inidicate lists of positive and negative words. These are located in your 
##"Day 14" Resources folder

positives= readLines("C:/Users/Student/Documents/DS4559/Day7/positive_words.txt")
negatives= readLines("C:/Users/Student/Documents/DS4559/Day7/negative_words.txt")

## Below is a function that scores sentiment on a scale of -5 to 5 (-5 being the most negative
## and 5 being the most positive).  A score is determined for each tweet based on its correlation
## with the positive words and the negative words.  This is original code written by a veteran R
##user that functions as part of an old package called "sentiment" that is no longer available.

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
  # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

## Now apply the function to our actual data.  

Score <- score.sentiment(some_txt,positives,negatives,.progress='none')

## Score has two fields: score and text.  We 
## are interested in score at this point, but we can look at a few of the tweets' text and
## the associated score first.

head(Score)

## Let’s plot a histogram of the sentiment score:

hist(Score$score,xlab="Sentiment Score ",main="Sentiment of sample tweets that have Acne Words in them ",
     border="black",col="skyblue")

## We can calculate overall sentiment by adding together all of the scores:

sum(Score$score)

#######################################################

#######Additional Analyses

#######################################################

##Some_tweets as a dataframe for bigrams and other analysis
tweet_DF <- twListToDF(some_tweets)


##$$$$$$$$$$$$$$$$$$$$$$

##Cleaning the text in the new Data Frame

tweet_DF$text = sapply(some_tweets, function(x) x$getText())
# remove retweet entities
tweet_DF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_DF$text)
# remove at people
tweet_DF$text = gsub("@\\w+", "", tweet_DF$text)
# remove punctuation
tweet_DF$text = gsub("[[:punct:]]", "", tweet_DF$text)
# remove numbers
tweet_DF$text = gsub("[[:digit:]]", "", tweet_DF$text)
# remove html links
tweet_DF$text = gsub("http\\w+", "", tweet_DF$text)
# remove unnecessary spaces
tweet_DF$text = gsub("[ \t]{2,}", "", tweet_DF$text)
tweet_DF$text = gsub("^\\s+|\\s+$", "", tweet_DF$text)
tweet_DF$text = gsub('???','',tweet_DF$text)
tweet_DF$text = gsub('etc','',tweet_DF$text)
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
# lower case using try.error with sapply 
tweet_DF$text = sapply(tweet_DF$text, try.error)

# remove NAs in tweet_DF$text
tweet_DF$text = tweet_DF$text[!is.na(tweet_DF$text)]

####$$$$$$$$$$$$$$$$$$$$

##Extracting the Dataframe of tweets for further analysis in excel
#As csv
#write.csv(tweet_DF, file = 'tweet_Data.csv')
#As xlsx
#library(xlsx)
#write.xlsx(tweet_DF, "C:/Users/Student/Documents/Capstone/tweet_DF.xlsx")
##$$$$$$$$$$$$$$$$$$$$$

##Exploratory Data Analysis

tweet_DF %>% filter(!is.na(latitude))

##Assigning Scores to Full tweets
tweet_DF <- cbind(tweet_DF, Score$score)
##$$$$$$$$$$$$$$$$$$$$$

##Unnesting for n_grams, tf_idf, and topic modeling
#By Word
tweet_words <- tweet_DF %>% tidytext::unnest_tokens(word, text) %>% mutate(created)
#By Bigram
tweet_bigrams <- tweet_DF %>% tidytext::unnest_tokens(bigram, text,token = "ngrams", n=2)

##Saving the unnested words into an excel file
#write.xlsx(tweet_words, "C:/Users/Student/Documents/Capstone/tweet_words.xlsx")

##Histogram of use of word lemon in tweets related to acne
tweet_words %>% filter(word=='lemon') %>% ggplot(aes(x=created)) + geom_histogram()

##Plot of favorite counts for key accounts
tweet_DF %>% group_by(screenName) %>% filter(favoriteCount > 100) %>% ggplot(aes(x = screenName, y=favoriteCount)) + geom_point()
##Favorite counts over time for key accounts
tweet_DF %>% group_by(screenName) %>% filter(favoriteCount > 40) %>% ggplot(aes(x=created, y = favoriteCount, col = screenName)) + geom_point()


tweet_words <- tweet_words %>% inner_join(get_sentiments('afinn'), by = c(word='word'))
write.csv(tweet_words, file = 'tweet_words1114.csv')
tweet_words %>% filter(word == 'lemon') %>% ggplot(aes(x=created, y = score))+ geom_point()

##Add sentiment scores for full tweets rather than just words specifically

