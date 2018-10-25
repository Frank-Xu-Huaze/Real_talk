# Reading samples
setwd("~/Desktop/")
sample <- read.csv("sample.csv", stringsAsFactors = FALSE)
sample <- sample[1:55,1:7]
colnames(sample)[3] <- "gender"
colnames(sample)[4] <- "LGBTQ"
colnames(sample)[5] <- "birth"
colnames(sample)[6] <- "zip"
colnames(sample)[7] <- "published"

# Creating Corpus
library(tm)
sample_corpus <- Corpus(VectorSource(as.vector(sample$Story))) 
sample_corpus <- tm_map(sample_corpus, removeWords, stopwords("english"))
sample_corpus <- tm_map(sample_corpus, content_transformer(removeNumbers))
sample_corpus <- tm_map(sample_corpus, content_transformer(removePunctuation))
sample_corpus <- tm_map(sample_corpus, content_transformer(tolower))
sample_corpus <- tm_map(sample_corpus, content_transformer(stemDocument), language = "english")

# Creating dtm
DTM <- DocumentTermMatrix(sample_corpus, control = list(wordLengths = c(2, Inf)))
TDM <- TermDocumentMatrix(sample_corpus, control = list(wordLengths = c(2, Inf)))

# Topic modeling
library(topicmodels)
library(tidytext)
library(dplyr)
library(ggplot2)

topic_model<-LDA(DTM, k=10, control = list(seed = 321))

# Tidying the model
topics <- tidy(topic_model, matrix = "beta")
top_terms <- 
  topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Ploting 
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Wordcloud
library("wordcloud")
library("RColorBrewer")
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
