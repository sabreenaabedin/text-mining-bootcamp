library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)


###### LOAD TEXTS #####

dox

##### BEGIN CLEANING #####
dox <- tm_map(dox,content_transformer(tolower)) 
dox <- tm_map(dox,stripWhitespace) 
dox <- tm_map(dox,removeWords,stopwords('english'))  #FREEZING ON THIS ONE
dox <- tm_map(dox,removePunctuation)
dox <- tm_map(dox,stemDocument)
dox <- tm_map(dox, removeNumbers)
inspect(dox)


dtm <- DocumentTermMatrix(dox)
tdm <- TermDocumentMatrix(dox)