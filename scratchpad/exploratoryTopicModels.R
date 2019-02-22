# install.packages('stm')
# install.packages('igraph')
# install.packages('stmCorrViz')
library(stm)        # Package for sturctural topic modeling
library(igraph)     # Package for network analysis and visualisation
library(stmCorrViz) # Package for hierarchical correlation view of STMs


file.name <- 'EPA'
d <- gs_title(file.name) %>% gs_read()


processed <- textProcessor(d$SUBJECT, metadata=d)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)

# save the output object meta, documents, and vocab into variables
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

# k <- 0 will use Lee and Mimno technique and choose # of topics(will change depending on seed)
k <- 0

fit <- stm(documents = out$documents, vocab = out$vocab, K = k, 
           max.em.its = 1000, data = out$meta, init.type = "Spectral")



thoughts3 <- findThoughts(fit, n = 2, texts =  , topics = 3)$docs[[1]]


estimateEffect(formula = 1:20 ~ rating + s(day), stmobj = fit,
               metadata = out$meta, uncertainty = "Global")



x <- labelTopics(fit, c(1:k))










