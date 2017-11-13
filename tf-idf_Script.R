install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
library(tidyverse)
physics <- gutenberg_download(c(37729, 14725,13476,5001), meta_fields = "author")
                              
##First tokens from the corpus are unnested
##Then Taking a count of author and word and sorting by author and word, then ungroup since we counted
physics_words <- physics %>%  unnest_tokens(word, text) %>%
  count(author, word, sort = TRUE) %>% ungroup()

typeof(physics_words)

##bind tf idf and arrange in descending order
physics_words %>%
  bind_tf_idf(word,author,n) %>%
  arrange(desc(tf_idf))

##Plot physics will now be ordered by tf-idf and will include word names and author names
plot_physics <- physics_words %>%
  bind_tf_idf(word, author, n) %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  mutate(author = factor(author, levels = c("Galilei, Galileo" ,
                                            "Huygens, Christiaan",
                                            "Tesla, Nikola",
                                            "Einstein, Albert")))
typeof(plot_physics)

##Here, we plot by author, showing the top 15 words in a bar plot. 
 plot_physics %>%
    group_by(author) %>%
    top_n(15, tf_idf) %>%
    ungroup() %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(word, tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    coord_flip()

  
   mystopwords <- data_frame(word = c("eq", "co", "k1","rc","ac","cg"))
  physics_words <-anti_join(physics_words, mystopwords, by = "word")
  typeof(physics_words)
  