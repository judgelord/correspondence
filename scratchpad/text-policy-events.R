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
# Creates duplicate rows for lines with multiple representatives
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



