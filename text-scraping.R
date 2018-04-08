install.packages("rvest")
library(rvest)

#write to file
setwd("/Users/sabreenaabedin/Desktop/automation-tools/")
sink("output.txt")

#### MY ISSUE IS GETTING THIS ARTICLE TITLE TO GO THROUGH ALL THE LINKS AVAILABLE
articleTitle

site <- html("https://www.trendhunter.com/results?search=skin+care")

links <- site %>%
  html_nodes("a") %>%
  html_attr("href")

df <-data.frame(link = character())
df$link <- links

for(i in 0:length(links)){
  test = substring(links[i],0,1)
  condition = compareEqual(test, "/")
  if(condition[1] == TRUE){
    ##df$link[i] <- links[i]
    print(links[i])
  } else {
    
  }
}

############ SCRAPE AND WRITE - THIS PART WORKS IF THERE'S A TITLE

for(i in 15:length(links)){
  name <- paste0("https://www.trendhunter.com", articleTitle)
site <- read_html(name)

#scrape html code
subTitle <- site %>%
  html_nodes(".subTitle > h2") %>%
  html_text() 

articleText <- site %>%
  html_nodes(".articleText") %>%
  html_text() 

title <- site %>%
  html_nodes(".titleSection > .cc") %>%
  html_text() 

author <- site %>%
  html_nodes(".author__link") %>%
  html_text() 

date <- site %>%
  html_nodes(".references > span") %>%
  html_text() 

# clean
author <- gsub("\n\t","", author)
author <- gsub("\t","", author)
articleText <- gsub("\n","", articleText)
articleText <- gsub("\t","", articleText)
title <- gsub("\n","", title)
title <- gsub("\t","", title)
subTitle <- gsub("\n","", subTitle)
subTitle <- gsub("\t","", subTitle)
date <- gsub("\n","", date)
date <- gsub("\t","", date)


cat(title,";", subTitle,";", author,";", date,";",articleText, "\n")
}
sink()





#install.packages("compare")
#library(compare)


#condition.result()
#condition[1]
