library(zipcode)
library(tidyverse)
library(usmap)
library(ggplot2)

# Building a zipprefix-state df
data(zipcode)
zipprefix <- zipcode
zipprefix$zip <- substr(zipprefix$zip, 0, 3)
zip <- zipprefix %>% 
  select(zip, state) %>% 
  distinct(zip, .keep_all = TRUE)

# Excluding AK and HI
#zip <- zip[zip$state != "AK" & zip$state != "HI",]
# Excluding PR and VI
#zip <- zip[zip$state != "PR" & zip$state != "VI",]
# Excluding other Islands
#zip <- zip[zip$state != "GU",]

# Reading samples
sample <- read.csv("sample.csv", stringsAsFactors = FALSE)
sample <- sample[1:55,1:7]
colnames(sample)[3] <- "gender"
colnames(sample)[4] <- "LGBTQ"
colnames(sample)[5] <- "birth"
colnames(sample)[6] <- "zip"
colnames(sample)[7] <- "published"

# Creat a new df with state and state count
samplestate <- merge(sample, zip, by = "zip")
samplecount <- samplestate %>% 
  group_by(state) %>% 
  count(state)

# Plotting
plot_usmap(data = samplecount, values = "n", lines = "red") + 
  labs(title = "US Counties", subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(colour = "black", fill = "lightblue"), legend.position = "right")
