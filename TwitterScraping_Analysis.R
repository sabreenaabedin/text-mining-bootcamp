
positives= readLines("/Users/sabreenaabedin/Desktop/text-mining-bootcamp/positive_words.txt")
negatives= readLines("/Users/sabreenaabedin/Desktop/text-mining-bootcamp/negative_words.txt")

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
library(tidytext)

########## GATHER TWEETS ##########

key='vzfbkFKrWI48jfCp8sEsLNv0E'
secret='kBw4o5YZyqe1kvapC4eixurzXN87qVwMG9ORiSsbZ063gJtARx'
access_token='708225866-xxOcvjumf6JNFMbT1BDzGlCuCxWOYcxLdjhBMeP0'
access_token_secret='WqnWo5HKaeIfCmkQ1mkKWO8hAt9JZVVNtf6jFOE5Byoee'

download.file(url="http://curl.haxx.se/ca/cacert.pem",
              destfile="/Users/sabreenaabedin/Desktop/text-mining-bootcamp/cacert.pem",
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
some_tweets = searchTwitter("acne", n=5, lang="en")

# gather all the dates
weeks <- 1:52
start.date <- as.Date("2017/01/01")
y <- start.date + (weeks - 1)*7
y

November1 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-11-05", until="2017-11-12")
November2 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-11-12", until="2017-11-19")

October1 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-10-01", until="2017-10-08")
October2 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-10-08", until="2017-10-15")
October3 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-10-15", until="2017-10-22")
October4 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-10-22", until="2017-10-29")
Octboer5 = searchTwitter(skincare_terms, n=10000,  lang="en", since="2017-10-29", until="2017-11-05")

allTweets <- list(November1)
allTweets <- append(allTweets, November2)
allTweets <- append(allTweets, October1)
allTweets <- append(allTweets, October2)
allTweets <- append(allTweets, October3)
allTweets <- append(allTweets, October4)
allTweets <- append(allTweets, October5)

########## CLEANING TEXT ##########

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())

some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt) # remove RTs
some_txt = gsub("@\\w+", " ", some_txt) # remove @
some_txt = gsub("[[:punct:]]", " ", some_txt) # remove punctuation
some_txt = gsub("[[:digit:]]", " ", some_txt) # remove numbers
some_txt = gsub("http\\w+", "", some_txt) # remove html links
some_txt = gsub("[ \t]{2,}", "", some_txt) # remove spaces
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
some_txt = gsub('???','',some_txt)
some_txt = gsub('etc','',some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL
#length(some_txt)
#write.csv(some_txt, 'acne_tweets.csv')

#col=brewer.pal(6,"Dark2")
#wordcloud(some_txt, min.freq=5, scale=c(5,2),rot.per = 0.25,
#         random.color=T, max.word=100, random.order=F,colors=col)



########## SENTIMENT ANALYSIS ##########

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
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
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

# Apply function
Score <- score.sentiment(some_txt,positives,negatives,.progress='none')

#head(Score)
## Let’s plot a histogram of the sentiment score:
#hist(Score$score,xlab="Sentiment Score ",main="Sentiment of sample tweets that have Acne Words in them ",
#     border="black",col="skyblue")

# We can calculate overall sentiment by adding together all of the scores:
sum(Score$score)


########## Additional Analyses ##########

##Some_tweets as a dataframe
tweet_DF <- twListToDF(some_tweets)

# cleaning text in dataframe - WAS ALREADY DONE TO SOME_TWEETS
{
# tweet_DF$text = sapply(some_tweets, function(x) x$getText())
# tweet_DF$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweet_DF$text) # remove retweet entities
# tweet_DF$text = gsub("@\\w+", " ", tweet_DF$text) #  remove at people
# tweet_DF$text = gsub("[[:punct:]]", " ", tweet_DF$text) # remove punctuation
# tweet_DF$text = gsub("[[:digit:]]", " ", tweet_DF$text) # remove numbers
# tweet_DF$text = gsub("http\\w+", "", tweet_DF$text) # remove html links
# tweet_DF$text = gsub("[ \t]{2,}", "", tweet_DF$text) # remove unnecessary spaces
# tweet_DF$text = gsub("^\\s+|\\s+$", "", tweet_DF$text)
# tweet_DF$text = gsub('???','',tweet_DF$text)
# tweet_DF$text = gsub('etc','',tweet_DF$text)
}

# define "tolower error handling" function 
try.error = function(x)
{
  y = NA
  try_error = tryCatch(tolower(x), error=function(e) e)
  if (!inherits(try_error, "error"))
    y = tolower(x)
  return(y)
}

# lower case using try.error with sapply 
tweet_DF$text = sapply(tweet_DF$text, try.error)

tweet_DF <- tweet_DF %>% filter(!is.na(tweet_DF$text)) # remove NAs

#write.csv(tweet_DF, file = 'tweet_Data.csv')
#library(xlsx)
#write.xlsx(tweet_DF, "C:/Users/Student/Documents/Capstone/tweet_DF.xlsx")


##Exploratory Data Analysis

tweet_DF %>% filter(!is.na(latitude))

# SCORES BY TWEET
tweet_DF <- cbind(tweet_DF, Score$score)


##Unnesting for n_grams, tf_idf, and topic modeling
tweet_words <- tweet_DF %>% tidytext::unnest_tokens(word, text) %>% mutate(created) #By Word
tweet_bigrams <- tweet_DF %>% tidytext::unnest_tokens(bigram, text,token = "ngrams", n=2) #By Bigram

#write.xlsx(tweet_words, "C:/Users/Student/Documents/Capstone/tweet_words.xlsx")

##Histogram of use of word lemon in tweets related to acne
#tweet_words %>% filter(word=='lemon') %>% ggplot(aes(x=created)) + geom_histogram()
##Plot of favorite counts for key accounts
#tweet_DF %>% group_by(screenName) %>% filter(favoriteCount > 100) %>% ggplot(aes(x = screenName, y=favoriteCount)) + geom_point()
##Favorite counts over time for key accounts
#tweet_DF %>% group_by(screenName) %>% filter(favoriteCount > 40) %>% ggplot(aes(x=created, y = favoriteCount, col = screenName)) + geom_point()


tweet_words <- tweet_words %>% inner_join(get_sentiments('afinn'), by = c(word='word'))
write.csv(tweet_words, file = 'tweet_words.csv')

# tweet_words %>% filter(word == 'lemon') %>% ggplot(aes(x=created, y = score))+ geom_point()
