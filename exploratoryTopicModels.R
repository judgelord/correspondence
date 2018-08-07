library(tm)
library(slam)
library(Matrix)
#install.packages('topicmodels')
library(topicmodels)
#install.packages('dplyr')
#install.packages('tidyverse')
#install.packages('glmnet')
#install.packages('SnowballC')

library(glmnet)


file.name <- 'EPA'
data <- gs_title(file.name) %>% gs_read()

## Code adapted from https://eight2late.wordpress.com/2015/09/29/a-gentle-introduction-to-topic-modeling-using-r/

## Comment Out ## For running smaller sample size
data %<>% filter(ID < 200)

data$SUBJECT <- as.factor(data$SUBJECT)

#create corpus from vector
content <- Corpus(VectorSource(data$SUBJECT))


# Preprocessing
content <-tm_map(content,content_transformer(tolower)) #Transform to lower case

#remove potentially problematic symbols
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern, ' ' , x))})
content <- tm_map(content, toSpace, '-')
content <- tm_map(content, toSpace, '"')
content <- tm_map(content, toSpace, '"')
content <- tm_map(content, toSpace, "'")
content <- tm_map(content, toSpace, "'")
content <- tm_map(content, toSpace, "?????")
content <- tm_map(content, toSpace, "?")
content <- tm_map(content, toSpace, "???")
content <- tm_map(content, toSpace, "?????")


#remove punctuation
content <- tm_map(content, removePunctuation)
#Strip digits
content <- tm_map(content, removeNumbers)
#remove stopwords
content <- tm_map(content, removeWords, stopwords('english'))
#remove whitespace
content <- tm_map(content, stripWhitespace)
# transform to stem words
content <- tm_map(content,stemDocument)

#writeLines(as.character(content[[1]]))

myStopwords <- c("can", "say","one","way","use",
                 "also","howev","tell","will",
                 "much","need","take","tend","even",
                 "like","particular","rather","said",
                 "get","well","make","ask","come","end",
                 "first","two","help","often","may",
                 "might","see","someth","thing","point",
                 "post","look","right","now","think","'ve ",
                 "'re ","anoth","put","set","new","good",
                 "want","sure","kind","larg","yes,","day","etc",
                 "quit","sinc","attempt","lack","seen","awar",
                 "littl","ever","moreov","though","found","abl",
                 "enough","far","earli","away","achiev","draw",
                 "last","never","brief","bit","entir","brief",
                 "great","lot")
content <- tm_map(content, removeWords, myStopwords)



# rowTotals <- apply(sdtm , 1, sum) #Find the sum of words in each Document
# sdtm.new   <- sdtm[rowTotals> 0, ]           #remove all docs without words



#Create document-term matrix
dtm <- DocumentTermMatrix(content)


rs = row_sums(dtm)
dtm = dtm[rs>0,]

#rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
#dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

#sdtm <- sparseMatrix(i = dtm$i, j = dtm$j)
#collapse matrix by summing over columns
freq <- colSums(as.matrix(dtm))

#create sort order (descending)
ord <- order(freq,decreasing=TRUE)
#List all terms in decreasing order of freq and write to disk
# freq[ord]
#write.csv(freq[ord],"word_freq.csv")



#Set parameters for Gibbs sampling
# burnin <- 4000
# iter <- 2000
# thin <- 500
# seed <-list(2003,5,63,100001,765)
# nstart <- 5
# best <- TRUE

#Number of topics
k <- 10

#Run LDA using VEM sampling
ldaOut <-LDA(dtm,k, method="VEM",  control = NULL, model = NULL)

#write out results
#docs to topics
ldaOut.topics <- as.matrix(topics(ldaOut))
#write.csv(ldaOut.topics,file=paste("LDAGibbs",k,"DocsToTopics.csv"))

#top 6 terms in each topic
ldaOut.terms <- as.matrix(terms(ldaOut,12))
#write.csv(ldaOut.terms,file=paste("LDAGibbs",k,"TopicsToTerms.csv"))

#probabilities associated with each topic assignment
topicProbabilities <- as.data.frame(ldaOut@gamma)
#write.csv(topicProbabilities,file=paste("LDAGibbs",k,"TopicProbabilities.csv"))

#Find relative importance of top 2 topics
topic1ToTopic2 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k]/sort(topicProbabilities[x,])[k-1])


#Find relative importance of second and third most important topics
topic2ToTopic3 <- lapply(1:nrow(dtm),function(x)
  sort(topicProbabilities[x,])[k-1]/sort(topicProbabilities[x,])[k-2])

#write.csv(topic1ToTopic2,file=paste("LDAGibbs",k,"Topic1ToTopic2.csv"))
#write.csv(topic2ToTopic3,file=paste("LDAGibbs",k,"Topic2ToTopic3.csv"))
