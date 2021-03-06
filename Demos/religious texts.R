##### LOAD DATA #####

# define local path
localpath <- "/Users/sabreenaabedin/Desktop/text-mining-bootcamp/files"

# install and library packages
install.packages("tm")
install.packages("wordcloud")
install.packages("SnowballC")
install.packages("ggplot2")
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)

# Create Corpus
# from a folder of texts
dox <- Corpus(DirSource(localpath),readerControl = list(language="eng"))

#from a single text file
temp <- readLines(paste(localpath, "/3-Quran-Islam.txt", sep=""))
quran <- Corpus(VectorSource(temp))

temp <- readLines(paste(localpath, "/2-King-James-Bible-Christianity.txt", sep=""))
bible <- Corpus(VectorSource(temp))

temp <- readLines(paste(localpath, "/1-Book-of-Mormon-Mormonism.txt", sep=""))
mormon <- Corpus(VectorSource(temp))

temp <- readLines(paste(localpath, "/4-Gospel-of-Budda-Buddhism.txt", sep=""))
buddha <- Corpus(VectorSource(temp))

temp <- readLines(paste(localpath, "/5-Zend-Avesta.txt", sep=""))
zoroastrian <- Corpus(VectorSource(temp))

temp <- readLines(paste(localpath, "/6-Meditations.txt", sep=""))
meditation <- Corpus(VectorSource(temp))

rm(temp) # clean workspace

alldocs <- list(dox, quran, bible, mormon, buddha, zoroastrian, meditation)

##### CLEANING AND PREPROCESSING DATA #####
clist <- as.list 
t <-alldocs[[i]]
t[1]
t <- tm_map(t, removeNumbers)

# for (txt in c("dox","quran","bible", "mormon", "buddha", "zoroastrian", "meditation")){
#   corp <- Corpus(VectorSource(txt))
#   corp <- tm_map(corp,content_transformer(tolower)) 
#   corp <- tm_map(corp,stripWhitespace) 
#   corp <- tm_map(corp,removeWords,stopwords('english')) 
#   corp <- tm_map(corp,removePunctuation)
#   corp <- tm_map(corp,stemDocument)
#   corp <- tm_map(corp, removeNumbers)
#   inspect(corp)
# }

{
# all documents
dox <- tm_map(dox,content_transformer(tolower)) 
dox <- tm_map(dox,stripWhitespace) 
dox <- tm_map(dox,removeWords,stopwords('english')) 
dox <- tm_map(dox,removePunctuation)
dox <- tm_map(dox,stemDocument)
dox <- tm_map(dox, removeNumbers)
inspect(dox)

#quran
quran <- tm_map(quran,content_transformer(tolower)) 
quran <- tm_map(quran, PlainTextDocument)
quran <- tm_map(quran,stripWhitespace) 
quran <- tm_map(quran,removeWords,stopwords('english')) 
quran <- tm_map(quran,removePunctuation)
quran <- tm_map(quran,stemDocument)
quran <- tm_map(quran, removeNumbers)

# bible
bible <- tm_map(bible,content_transformer(tolower)) 
bible <- tm_map(bible, PlainTextDocument)
bible <- tm_map(bible,stripWhitespace) 
bible <- tm_map(bible,removeWords,stopwords('english')) 
bible <- tm_map(bible,removePunctuation)
bible <- tm_map(bible,stemDocument)
bible <- tm_map(bible, removeNumbers)

# mormon
mormon <- tm_map(mormon,content_transformer(tolower)) 
mormon <- tm_map(mormon, PlainTextDocument)
mormon <- tm_map(mormon,stripWhitespace) 
mormon <- tm_map(mormon,removeWords,stopwords('english')) 
mormon <- tm_map(mormon,removePunctuation)
mormon <- tm_map(mormon,stemDocument)
mormon <- tm_map(mormon, removeNumbers)

# buddha
buddha <- tm_map(buddha,content_transformer(tolower)) 
buddha <- tm_map(buddha, PlainTextDocument)
buddha <- tm_map(buddha,stripWhitespace) 
buddha <- tm_map(buddha,removeWords,stopwords('english')) 
buddha <- tm_map(buddha,removePunctuation)
buddha <- tm_map(buddha,stemDocument)
buddha <- tm_map(buddha, removeNumbers)

# zoroastrian
zoroastrian <- tm_map(zoroastrian,content_transformer(tolower)) 
zoroastrian <- tm_map(zoroastrian, PlainTextDocument)
zoroastrian <- tm_map(zoroastrian,stripWhitespace) 
zoroastrian <- tm_map(zoroastrian,removeWords,stopwords('english')) 
zoroastrian <- tm_map(zoroastrian,removePunctuation)
zoroastrian <- tm_map(zoroastrian,stemDocument)
zoroastrian <- tm_map(zoroastrian, removeNumbers)

# meditation
meditation <- tm_map(meditation,content_transformer(tolower)) 
meditation <- tm_map(meditation, PlainTextDocument)
meditation <- tm_map(meditation,stripWhitespace) 
meditation <- tm_map(meditation,removeWords,stopwords('english')) 
meditation <- tm_map(meditation,removePunctuation)
meditation <- tm_map(meditation,stemDocument)
meditation <- tm_map(meditation, removeNumbers)
}

# create a document term matrix
dtm <- DocumentTermMatrix(dox)
#view summary 
dtm
tdm <- TermDocumentMatrix(dox)
tdm
inspect(dtm[1:5, 1:5])


tdm.common <- removeSparseTerms(tdm, .1)
dtm.common <-removeSparseTerms(dtm, 0.6)
inspect(dtm.common)

findAssocs(dtm, "god", 0.99)
findAssocs(dtm, "data", corlimit=0.6)

#dtm1 <- DocumentTermMatrix(dox[2:3])

## Word Clouds 
{
  
  # all terms
  wordcloud(dox,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  # quran
  wordcloud(quran,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  # bible
  wordcloud(bible,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  # mormon
  wordcloud(mormon,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  # buddha
  wordcloud(buddha,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  # zoroastrian
  wordcloud(zoroastrian,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  # meditation
  wordcloud(meditation,scale=c(5,0.5),max.words=80, random.order = FALSE, rot.per = .25, colors = RColorBrewer::brewer.pal(8,"Dark2"))
  
} 

## Word Count
{

inspect(dtm[1:3,1:3]) #verify that sum of the rows would give total number of words
rowTotals <- apply(dtm, 1, sum)
View(rowTotals)
barplot(rowTotals, main="Terms per Document", xlab = "Word Count",
        ylab="Document", col="darksalmon", horiz = TRUE, 
        names.arg=c("Mormon", "Bible", "Quran", "Buddh.", "Zend.", "Med."))
}

## Cluster Dendrograms
{

findFreqTerms(dtm,100)
freq <- colSums(as.matrix(dtm)) 
ord <- order(freq,decreasing=FALSE) # terms in ascending order
fterms <- freq[tail(ord, 20)] # grab last 20 terms
tail(fterms) # verify

my.df <- as.data.frame(fterms)
my.df.scale <- scale(my.df) #normalize
d <- dist(my.df.scale,method="euclidean") #find euclidean distance
fit <- hclust(d, method="ward.D")
plot(fit, col = "indianred4", xlab = "Terms") #plot

 ## k means clustering
m <- as.matrix(dtm)
d <- dist(m)
groups <- hclust(d, method="ward.D")
plot(groups, hang = -1)  
rect.hclust(groups,2)
rect.hclust(groups, 4) # k = 4

# dtmq <- DocumentTermMatrix(quran) 
# m <- as.matrix(dtmq)
# d <- dist(m)
# groups <- hclust(d, method="ward.D")
# plot(groups, hang = -1)  
# rect.hclust(groups,2)
# rect.hclust(groups, 4) # k = 4

# dtmb <- DocumentTermMatrix(bible)
# m <- as.matrix(dtmb)
# d <- dist(m)
# groups <- hclust(d, method="ward.D")
# plot(groups, hang = -1)  
# rect.hclust(groups,2)
# rect.hclust(groups, 4) # k = 4
# 
# dtmbud <- DocumentTermMatrix(buddha)
# m <- as.matrix(dtmbud)
# d <- dist(m)
# groups <- hclust(d, method="ward.D")
# plot(groups, hang = -1)  
# rect.hclust(groups,2)
# rect.hclust(groups, 4) # k = 4

# dtmzor <- DocumentTermMatrix(zoroastrian)
# m <- as.matrix(dtmzor)
# d <- dist(m)
# groups <- hclust(d, method="ward.D")
# plot(groups, hang = -1)  
# rect.hclust(groups,2)
# rect.hclust(groups, 4) # k = 4
}

## Sentiment Analysis
{
  positives= readLines("C:/Users/Sabreena/Dropbox/DS/text_mining/positive_words.txt")
  negatives= readLines("C:/Users/Sabreena/Dropbox/DS/text_mining/negative_words.txt")
  score.sentiment = function(sentences, pos.words, neg.words)
  {
    require(plyr)
    require(stringr)
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
    }, pos.words, neg.words)
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  # create data frame to track scores
  SentimentScores <- as.data.frame(c("Mormon", "Bible", "Quran", "Buddh.", "Zend.", "Med."))
  SentimentScores[c("scores")] <- NA
  View(SentimentScores)
  
  # Book of Mormon
  Mormon <- readLines("C:/Users/Sabreena/Dropbox/DS/final/1-Book-of-Mormon-Mormonism.txt")
  Score <- score.sentiment(Mormon,positives,negatives)
  hist(Score$score,xlab="Sentiment Score ",main="Mormon Sentiment",
       border="black",col="darkseagreen")
  SentimentScores[1,2] <- sum(Score$score)
  
  # Bible
  Bible <- readLines("C:/Users/Sabreena/Dropbox/DS/final/2-King-James-Bible-Christianity.txt")
  Score <- score.sentiment(Bible,positives,negatives)
  hist(Score$score,xlab="Sentiment Score ",main="Bible Sentiment",
       border="black",col="darkseagreen")
  SentimentScores[2,2] <- sum(Score$score)
  
  # Quran
  Quran <- readLines("C:/Users/Sabreena/Dropbox/DS/final/3-Quran-Islam.txt")
  Score <- score.sentiment(Quran,positives,negatives)
  hist(Score$score,xlab="Sentiment Score ",main="Quran Sentiment",
       border="black",col="darkseagreen")
  SentimentScores[3,2] <- sum(Score$score)
  
  # Buddhism
  Buddh <- readLines("C:/Users/Sabreena/Dropbox/DS/final/4-Gospel-of-Budda-Buddhism.txt")
  Score <- score.sentiment(Buddh,positives,negatives)
  hist(Score$score,xlab="Sentiment Score ",main="Buddha Sentiment",
       border="black",col="darkseagreen")
  SentimentScores[4,2] <- sum(Score$score)
  
  # Zend Avesta
  Zend <- readLines("C:/Users/Sabreena/Dropbox/DS/final/5-Zend-Avesta-zoroastrianism-NEW.txt")
  Score <- score.sentiment(Zend,positives,negatives)
  hist(Score$score,xlab="Sentiment Score ",main="Zend Sentiment",
       border="black",col="darkseagreen")
  SentimentScores[5,2] <- sum(Score$score)
  
  #Meditations
  Med <- readLines("C:/Users/Sabreena/Dropbox/DS/final/6-Meditations.txt")
  Score <- score.sentiment(Med,positives,negatives)
  hist(Score$score,xlab="Sentiment Score ",main="Meditation Sentiment",
       border="black",col="darkseagreen")
  SentimentScores[6,2] <- sum(Score$score)
  
  View(SentimentScores)
  plot(SentimentScores, horiz = TRUE, col = "magenta")
  
  # divide by the word count
  SentimentScores[1,2] <- SentimentScores[1,2]/126447
  SentimentScores[2,2] <- SentimentScores[2,2]/373701
  SentimentScores[3,2] <- SentimentScores[3,2]/93121
  SentimentScores[4,2] <- SentimentScores[4,2]/45157
  SentimentScores[5,2] <- SentimentScores[5,2]/91421
  SentimentScores[6,2] <- SentimentScores[6,2]/35283
  
  View(SentimentScores)
  plot(SentimentScores)
}

## frequency histogram - attempted
{
  # wf=data.frame(term=names(freq),occurrences=freq)
  # View(wf)
  # p <- ggplot(subset(wf, freq>3000), aes(term, occurrences))
  # p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
  # p
}

## violence in religious texts
# timothyrenner.github.io
{
  # violentwords <- data.frame(c("wound","hurt","fight","violate","destroy",
  #                              "slaughter", "murder", "kill", "attack", "break",
  #                              "crush", "provoke", "anger", "hatred"))
  # colnames(violentwords) <- "words"
  # View(violentwords)
  
  findFrequency = function(text)
  {
    ## attempted iterative approach at first
    # violentfrequency <- 0
    # for(i in 1:nrow(data.frame)){
      # n <- length(grep(text, data.frame[i]))
      # violentfrequency <- violentfrequency + n
    # }
    
    ## ended up hard coding the violent words
    violentfrequency <- 0
    violentfrequency <- length(grep("wound", text)) + length(grep("hurt", text)) + 
      length(grep("fight", text)) + length(grep("violate", text)) + length(grep("destroy", text)) +
      length(grep("slaughter", text)) + length(grep("murder", text)) + length(grep("kill", text))
    + length(grep("attack", text)) + length(grep("break", text)) + length(grep("crush", text))
    + length(grep("provoke", text)) + length(grep("anger", text)) + length(grep("hatred", text))
    
    return(violentfrequency)
  }
  
  findFrequency(Quran)
  #check
  length(grep("wound", Quran))
  length(grep("hurt", Quran))
  
  # create data frame to track scores
  violence <- as.data.frame(c("Mormon", "Bible", "Quran", "Buddh.", "Zend.", "Med."))
  violence[c("scores")] <- NA
  View(violence)
  
  #calculate scores
  violence[1,2] <- findFrequency(Mormon)
  violence[2,2] <- findFrequency(Bible)
  violence[3,2] <- findFrequency(Quran)
  violence[4,2] <- findFrequency(Buddh)
  violence[5,2] <- findFrequency(Zend)
  violence[6,2] <- findFrequency(Med)
  
  View(violence)
  plot(violence, xlab = "text")
  
  
  # divide by the word count
  violence[1,2] <- violence[1,2]/126447
  violence[2,2] <- violence[2,2]/373701
  violence[3,2] <- violence[3,2]/93121
  violence[4,2] <- violence[4,2]/45157
  violence[5,2] <- violence[5,2]/91421
  violence[6,2] <- violence[6,2]/35283
  
  View(violence)
  plot(violence, xlab = "text")
  
}

## graph viz - required RGraphViz which uninstalled all my other libraries
{
# plot(tdm,
#      terms = sample(fterms, 10),
#      corThreshold = 0.7,
#      weighting = FALSE,
#      attrs = list(graph = list(rankdir = "BT"),
#                   node = list(shape = "rectangle",
#                               fixedsize = FALSE))
#    )
}

## log 
{
# dtm.dense <- as.matrix(dtm)
# dim(dtm.dense)
# library(reshape2)
# dtm.dense = melt(dtm.dense, value.name = "count")
# 
# ggplot(dtm.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
#       geom_tile(colour = "white") +
#       scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
#       ylab("") +
#       theme(panel.background = element_blank()) +
#       theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
# 
# ggplot(dtm.dense, aes(x = Docs, y = Terms))
}

# library(cluster)   
# d <- dist(t(dtm), method="euclidian")   
# fit <- hclust(d=d, method="ward")   
# fit
# plot(fit,hang = -1)
# 
# library(fpc)
# dtms <- removeSparseTerms(dtm, 0.60) # Prepare the data (max 15% empty space)   
# d <- dist(t(dtms), method="euclidian")   
# kfit <- kmeans(d, 4)    # 2 groups
# clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

