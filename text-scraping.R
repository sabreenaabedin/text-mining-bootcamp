################################
### INSTALL/LIBRARY PACKAGES ###
################################
install.packages("rvest")
install.packages("stringr")
library(rvest)
library(stringr)

############################
### FIND LINKS TO FOLLOW ###
############################

# DEFINE PAGE ROOT
main.page <- read_html(x = "https://www.trendhunter.com/results?search=skin+care")

# CREATING LIST OF LINKS
urls <- main.page %>% 
  html_nodes("a") %>% 
  html_attr("href") # extract the URLs

links <- main.page %>% 
  html_nodes("a") %>% 
  html_text() # extract the link text

# Combine `links` and `urls` into a data.frame
  #linkframe <- data.frame(links = links, urls = urls, stringsAsFactors = FALSE)
linkframe <- data.frame(urls = urls, stringsAsFactors = FALSE)
head(linkframe)
linkframe <- linkframe[!is.na(linkframe)]


pattern <- "/." # PATTERN INDICATES IT IS A LINK
links_valid <- character(0) # create vector for valid links

# how many are valid links?
for(i in 1:length(linkframe)){
  print(str_detect(linkframe[i], pattern))
  if(str_detect(linkframe[i], pattern)){
    links_valid <- c(linkframe[i])
    print(paste0("Added link: ",linkframe[i])) #this part prints the links as needed
  }
}

head(links_valid) #RETURNS A RANDOM LINK?????


################################
### SCRAPE AND WRITE TO FILE ###
################################

#write to file
setwd("/Users/sabreenaabedin/Desktop/automation-tools/")
sink("output.txt")

### SCRAPE AND WRITE - THIS PART WORKS IF THE NAME IS CORRECT

for(i in 0:length(links_final)){
  name <- paste0("https://www.trendhunter.com", links_final[i]) #IF INVALID, WON'T WORK
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
