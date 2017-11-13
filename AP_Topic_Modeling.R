 ## Open all necessary libraries
library(topicmodels)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)

data("AssociatedPress")
AssociatedPress

## Note AssociatedPress is in a dtm format.

## Now we set a seed so that the output of the model is predictable:
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed=1234))
ap_lda

## Let's tidy this:

ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

## Let's use dplyr's top_n to find the 10 terms that are most common within each topic.

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") + 
  coord_flip()

## Now let's consider the topics that have the greatest difference between topic 1 and topic 2, using a 
## log2 ratio of the two: log2(beta1/beta2)

beta_spread <- ap_topics %>% mutate(topic = paste0("topic",topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/topic1))
         
         
beta_spread

## See if you can create a visualization of the top terms vs. the log2 ratio of beta
         