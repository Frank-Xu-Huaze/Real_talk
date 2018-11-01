library(zipcode)
library(tidyverse)
library(usmap)
library(ggplot2)
library(tidytext)
library(tm)
library(topicmodels)
library(wordcloud)
library(RColorBrewer)

# Reading samples
sample <- read.csv("Real_Talk_Data.csv", stringsAsFactors = FALSE)
sample <- sample[1:811,]
colnames(sample)[13] <- "zip"
sample$zip[sample$zip < 10000 & is.na(sample$zip) == F] <- 
  sprintf("%05d", sample$zip[sample$zip < 10000 & is.na(sample$zip) == F])
sample <- sample[ , colSums(!is.na(sample)) != 0]
sample$Story <- do.call(paste, c(sample[18:127], sep=" "))
sample <- sample[,c(1:17,128)]

# Using Tidytext to unnest the dataframe
sampledf <- sample %>% 
  select(Story) %>% 
  unnest_tokens("word", Story)
# Stopwords
data("stop_words")
samplesort <- sampledf %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))

# Graphing1: top 20 words in histogram
top_20 <- samplesort[1:20,]
top_20$word <- factor(top_20$word,
                      levels = top_20$word[order(top_20$n, decreasing = TRUE)])
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Word freq in real talks sample")+
  xlab("")+
  guides(fill=FALSE)

# Graphing2: Corpus & Topic modeling
# Creating Corpus
sample_corpus <- Corpus(VectorSource(as.vector(sample$Story))) 
sample_corpus <- tm_map(sample_corpus, removeWords, stopwords("english"))
sample_corpus <- tm_map(sample_corpus, content_transformer(removeNumbers))
sample_corpus <- tm_map(sample_corpus, content_transformer(removePunctuation))
sample_corpus <- tm_map(sample_corpus, content_transformer(tolower))
sample_corpus <- tm_map(sample_corpus, content_transformer(stemDocument), language = "english")
# Creating dtm
DTM <- DocumentTermMatrix(sample_corpus, control = list(wordLengths = c(2, Inf)))
TDM <- TermDocumentMatrix(sample_corpus, control = list(wordLengths = c(2, Inf)))
topic_model<-LDA(DTM, k=10, control = list(seed = 321)) #### Not Working !!!!!!
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

# Graphing3: Wordcloud
m <- as.matrix(TDM)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Graphing4: visualization of state counts for the sample
# Building a zipprefix-state df
data(zipcode)
#zipprefix <- zipcode
#zipprefix$zip <- substr(zipprefix$zip, 0, 3) # If only using prefix 
zip <- zipcode %>% 
  select(zip, state) %>% 
  distinct(zip, .keep_all = TRUE)
# Creat a new df with state and state count
samplestate <- merge(sample, zip, by = "zip")
samplecount <- samplestate %>% 
  group_by(state) %>% 
  count(state)
# Plotting
plot_usmap(regions = "states", data = samplecount, values = "n", lines = "red") + 
  scale_fill_continuous(low = "white", high = "red", name = "Population (2015)", label = scales::comma) +
  labs(title = "US States", subtitle = "This is a map.") + 
  theme(panel.background = element_rect(colour = "black", fill = "white"), legend.position = "right")


