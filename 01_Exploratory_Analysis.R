library(tidytext)
library(dplyr)
library(readr)
library(ggplot2)
set.seed(1235)

#Read in the text file data sources.
#https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
textBlogs <- read_lines("./final/en_US/en_US.blogs.txt")
textNews <- read_lines("./final/en_US/en_US.news.txt")
textTwitter <- read_lines("./final/en_US/en_US.twitter.txt")

#Convert to data frames for tidying.
dfBlogs <- data_frame(line = 1:length(textBlogs), text = textBlogs)
dfNews <- data_frame(line = 1:length(textNews), text = textNews)
dfTwitter <- data_frame(line = 1:length(textTwitter), text = textTwitter)

#Create tokenized data frames.
tidyBlogs <- unnest_tokens(dfBlogs, word, text)
tidyNews <- unnest_tokens(dfNews, word, text)
tidyTwitter <- unnest_tokens(dfTwitter, word, text)

#Profanity filter resource: https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
textProfanity <- read_lines("./en_profanity.txt")
dfProfanity <- data_frame(word = textProfanity)

#Remove profanity matches
tidyBlogs <- tidyBlogs %>% anti_join(dfProfanity)
tidyNews <- tidyNews %>% anti_join(dfProfanity)
tidyTwitter <- tidyTwitter %>% anti_join(dfProfanity)

#Word frequency data exploration
tidyBlogs %>% count(word, sort = TRUE)
tidyBlogs %>% anti_join(stop_words) %>% count(word, sort = TRUE)
tidyNews %>% count(word, sort = TRUE)
tidyNews %>% anti_join(stop_words) %>% count(word, sort = TRUE)
tidyTwitter %>% count(word, sort = TRUE)
tidyTwitter %>% anti_join(stop_words) %>% count(word, sort = TRUE)

#Find an appropriate sample size.
#Sample size of 85% covers a little over 90% of the words.
sampleSize <- .85

sampleBlogs <- sample_frac(dfBlogs, size = sampleSize)
sampleTidyBlogs <- unnest_tokens(sampleBlogs, word, text)
sampleTidyBlogs <- sampleTidyBlogs %>% anti_join(dfProfanity)

sampleNews <- sample_frac(dfNews, size = sampleSize)
sampleTidyNews <- unnest_tokens(sampleNews, word, text)
sampleTidyNews <- sampleTidyNews %>% anti_join(dfProfanity)

sampleTwitter <- sample_frac(dfTwitter, size = sampleSize)
sampleTidyTwitter <- unnest_tokens(sampleTwitter, word, text)
sampleTidyTwitter <- sampleTidyTwitter %>% anti_join(dfProfanity)

nrow(distinct(sampleTidyBlogs, word)) / nrow(distinct(tidyBlogs, word))
nrow(distinct(sampleTidyNews, word)) / nrow(distinct(tidyNews, word))
nrow(distinct(sampleTidyTwitter, word)) / nrow(distinct(tidyTwitter, word))

#2-gram 3-gram data exploration
bigramBlogs <- dfBlogs %>% unnest_tokens(bigram, text, token = "ngrams", n = 2)

bigramBlogs %>% count(bigram, sort = TRUE)


