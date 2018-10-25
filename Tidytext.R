# Reading samples
setwd("~/Desktop/")
sample <- read.csv("sample.csv", stringsAsFactors = FALSE)
sample <- sample[1:55,1:7]
colnames(sample)[3] <- "gender"
colnames(sample)[4] <- "LGBTQ"
colnames(sample)[5] <- "birth"
colnames(sample)[6] <- "zip"
colnames(sample)[7] <- "published"
#temp1 <- iconv(sample$Story, "latin1", "ASCII", "byte")

#Using Tidytext to unnest the dataframe
library(tidytext)
library(dplyr)
sampledf <- sample %>% 
  select(Story, gender, LGBTQ, birth, zip, published) %>% 
  unnest_tokens("word", Story)

#Stopwords
data("stop_words")
samplesort <- sampledf %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))

#Graphing
top_20 <- samplesort[1:20,]
#Factorizing in order to sort bars
top_20$word <- factor(top_20$word,
                      levels = top_20$word[order(top_20$n, decreasing = TRUE)])
library(ggplot2)
ggplot(top_20, aes(x=word, y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Word freq in real talks sample")+
  xlab("")+
  guides(fill=FALSE)

#Dictionary
sample_dict <- c("gay")
library(stringr)
sample2 <- sampledf[str_detect(sampledf$word, sample_dict),]
head(sample2$word)
