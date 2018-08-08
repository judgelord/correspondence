# install.packages('stm')
# install.packages('igraph')
# install.packages('stmCorrViz')
library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs


file.name <- 'EPA'
data <- gs_title(file.name) %>% gs_read()


processed <- textProcessor(data$SUBJECT, metadata=data)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# save the output object meta, documents, and vocab into variables
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

k <- 15

fit <- stm(documents = out$documents, vocab = out$vocab, K = 15, 
           max.em.its = 75, data = out$meta, init.type = "Spectral")



thoughts3 <- findThoughts(fit, n = 2, texts =  , topics = 3)$docs[[1]]


estimateEffect(formula = 1:20 ~ rating + s(day), stmobj = fit,
               metadata = out$meta, uncertainty = "Global")



labelTopics(fit, c(1:k))










