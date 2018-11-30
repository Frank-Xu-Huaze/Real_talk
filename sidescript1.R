library(pluralize)
library(tidyverse)
library(tidytext)

# Reading samples
setwd("~/Desktop/Real_talk/")
sample <- read.csv("Real_Talk_Data.csv", stringsAsFactors = FALSE)
sample <- sample[1:811,]
sample <- sample[ , colSums(!is.na(sample)) != 0]
sample$Story <- do.call(paste, c(sample[18:127], sep=" "))
sample <- sample[,c(1:17, 128)]

sampleword <- sample %>% 
  unnest_tokens("word", Story)

data("stop_words")
samplesort <- sampleword %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  arrange(desc(n))

samplesort[,1] <- singularize(samplesort$word)
samplesortcp <- samplesort

# Sum up the duplicates and remove any word with ""
for (word in samplesort$word){
  count = dim(samplesort[samplesort$word == word,])[1]
  if (count > 1){
    samplesort[samplesort$word == word,][1,2] = sum(samplesort[samplesort$word == word,][,2])
    c = 1
    for (i in a <- samplesort$word == word){
      if (i == T){
        max = c
      }
      c = c + 1
    }
    samplesort <- samplesort[-max, ]
  }
  if (grepl("’", word)){
    samplesort <- samplesort[samplesort$word != word, ]
  }
}

# Re-arrange
samplesort <- samplesort %>%  arrange(desc(n))

# The list of not useful words
unuseful <- c(2, 10, 12, 15, 22, 26, 36, 45, 51, 53, 
            54, 56, 57, 62, 63, 65, 66, 68, 69, 70, 
            74:76, 80, 81, 88, 92, 94, 96, 103, 104, 
            106, 108, 109, 110, 114, 116, 117, 118, 119, 
            122, 126:128, 130, 131, 135, 136, 138, 139, 
            142, 144, 147, 148, 151, 158, 159, 161, 162, 
            166, 177, 178, 182, 185, 189, 192:195, 198, 202)

samplesort <- samplesort[-unuseful, ]

write.table(samplesort[1:131, ], "hifreq.txt", sep=",")
