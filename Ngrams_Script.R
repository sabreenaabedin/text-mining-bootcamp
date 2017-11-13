library(dplyr)
library(tidytext)
library(janeaustenr)
?janeaustenr
austen_bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

austen_bigrams %>% count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_separated

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

# new bigram counts
bigram_counts <- bigrams_filtered %>%
  dplyr::count(word1, word2, sort = TRUE) 

bigram_counts

bigrams_united <- bigrams_filtered %>% 
  unite(bigram, word1, word2, sep = " ")

bigrams_united

### REALLY COOL aside: The one-bigram row format can be really helpful in exploratory
### analysis.  For example, maybe we want to know the most common streets mentioned
### in each book:

bigrams_filtered %>% filter(word2 == "street") %>%
  dplyr::count(book, word1, sort = TRUE)

## A bigram can also be treated as a term in a 
## document in the same way that we treated individual words.  
## For example, we can look st the tf-idf of bigrams across Austen novels.

bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

## ONE BIG PROBLEM with sentiment analysis as we've studied it to date, is that it does
## not take simple negation into account: Let's see how often sentiment-associated
## words are preceded by "not."

bigrams_separated %>%
  filter(word1 == "not") %>%
           dplyr::count(word1, word2, sort = TRUE)

## So sentiment analysis on the bigram data would allow us to ignore or reverse sentiment
## associated words preceded by "not or other negating words!!

AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>% 
  filter(word1 == "not") %>% 
  inner_join(AFINN, by = c(word2 = "word")) %>%
  dplyr::count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

## We can generalize to more negation words:

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>% inner_join(AFINN, by = c(word2 = "word")) %>%
  dplyr::count(word1, word2, score, sort = TRUE) %>%
          ungroup()

negated_words %>%
  mutate(contribution = n * score ) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2=reorder(word2,contribution)) %>%
  ggplot(aes(word2, n* score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) + 
  xlab("Words preceded by negations") +
  ylab("Sentiment score * number of occurrences") + coord_flip()

## And yet one more cool visual . . .
## Let's visualize a network of bigrams!!!

install.packages("igraph")
library(igraph)

# original counts
bigram_counts

## Filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

install.packages("ggraph")
library(ggraph)

ggraph(bigram_graph, layout = "fr") +
         geom_edge_link() +
        geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) 
           