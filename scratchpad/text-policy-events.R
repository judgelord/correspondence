list.of.packages <- c("tidytext", "topicmodels","textfeatures","cleanNLP")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(tidytext)
library(topicmodels)
library(textfeatures)
library(cleanNLP) ## https://statsmaths.github.io/cleanNLP/ 

load(url("https://github.com/judgelord/correspondence/raw/master/data/correspondenceTexts.Rdata"))
d <- correspondenceTexts 

# Naming things! 
d %<>% rename(Party = party_name)


###############    
# Creates duplicate rows for lines with multiple policy event classifications
for(i in 1:nrow(d)){
  if(grepl(";|/", d$POLICY_EVENT[i])) {
    
    new <- d %>% dplyr::slice(rep(i, each = str_count(d$POLICY_EVENT[i], pattern = ";|/") + 1))
    new$POLICY_EVENT <- unlist(str_split(d$POLICY_EVENT[i], ";|/"))
    
    d <- rbind(d, new)
    
  }
}
d <- d[-grep(";|/", d$POLICY_EVENT),] # removes orginal row with all data
d$POLICY_EVENT <- gsub("^ |^  | $|  $", "", d$POLICY_EVENT)
################



d %<>%
  mutate(POLICY_EVENT = str_to_upper(POLICY_EVENT)) %>% 
  mutate(POLICY_EVENT = ifelse(grepl("ALLOCATION",POLICY_EVENT), "BUDGET ALLOCATION", POLICY_EVENT)) 

z <- d %>%
  group_by(POLICY_EVENT) %>% 
  summarise(n = n()) %>% filter(n > 10) 

d %<>%
  filter(POLICY_EVENT %in% z$POLICY_EVENT)

words <- d %>% 
  unnest_tokens(word, SUBJECT) %>% 
  filter(!(word %in% stop_words$word)) %>% 
  group_by(POLICY_EVENT) %>%
  count(word, sort = TRUE) %>% 
  top_n(10) %>% 
  mutate(word = fct_inorder(word))


ggplot(words, aes(x = fct_rev(word), y = n)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Count", x = NULL, title = "10 most frequent words") +
  facet_wrap("POLICY_EVENT", scales = "free")




bigrams <- d %>% 
  group_by(POLICY_EVENT) %>% 
  unnest_tokens(bigram, SUBJECT, token = "ngrams", n = 2) %>% 
  # Split the bigram column into two columns
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>% 
  # Put the two word columns back together
  unite(bigram, word1, word2, sep = " ") %>% 
  count(bigram, sort = TRUE) %>% 
  top_n(5)

ggplot(bigrams, aes(x = reorder(bigram, n), y = n)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Count", x = NULL, title = "10 most frequent word pairs") +
  facet_wrap("POLICY_EVENT", scales = "free")


# "hearing entitled" --> hearing
# grep("\\(grant support\\)|\\[grant support\\]",d$SUBJECT,ignore.case = T) --> grant (earmark?)
# grep("meeting request",d$SUBJECT,ignore.case = T) --> meeting 
# grep("requests information",d$SUBJECT,ignore.case = T) --> information
# grep("hearing entitled",d$SUBJECT,ignore.case = T) --> hearing



trigrams <- d %>% 
  group_by(POLICY_EVENT) %>% 
  unnest_tokens(trigram, SUBJECT, token = "ngrams", n = 3) %>% 
  # Split the trigram column into three columns
  separate(trigram, c("word1", "word2","word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  # Put the three word columns back together
  unite(trigram, word1, word2, word3, sep = " ") %>% 
  count(trigram, sort = TRUE) %>% 
  top_n(5)

ggplot(trigrams, aes(x = reorder(trigram, n), y = n)) + 
  geom_col() + 
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Count", x = NULL, title = "10 most frequent word pairs") +
  facet_wrap("POLICY_EVENT", scales = "free")


# Term Frequency - Inverse Document Frequency

# Get a list of words
words <- d %>% 
  unnest_tokens(word, SUBJECT) %>% 
  group_by(POLICY_EVENT) %>% 
  count(word, sort = TRUE) %>% 
  ungroup()

# Add the tf-idf for these words
tf_idf <- words %>% 
  bind_tf_idf(word, POLICY_EVENT, n) %>% 
  arrange(desc(tf_idf))

# Get the top 10 most unique words
tf_idf %>% 
  group_by(POLICY_EVENT) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  # order by word
  mutate(word = fct_inorder(word)) %>%
  # Plot by tf_idf
  ggplot(aes(x = fct_rev(word), y = tf_idf, fill = POLICY_EVENT)) +
  geom_col() +
  guides(fill = FALSE) +
  labs(y = "tf-idf", x = NULL) +
  facet_wrap(~ POLICY_EVENT, scales = "free") +
  coord_flip()







