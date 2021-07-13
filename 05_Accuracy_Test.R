library(readr)
library(dplyr)
library(quanteda)

#Read in test set.
testSet <- read_lines("./datasets/TestBlogs.txt")
testSet <- c(testSet, read_lines("./datasets/TestNews.txt"))
testSet <- c(testSet, read_lines("./datasets/TestTwitter.txt"))

#Create a sub-sample for speed.
set.seed(9077)
testSet <- sample(testSet, 10000)

#Profanity filter resource: https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
profanity <- read_lines("./en_profanity.txt")

#Tokenize and create n-grams for testing.
testQuintgrams <- testSet %>% corpus() %>% corpus_reshape("sentence") %>%
	tokens(remove_punct = TRUE) %>% tokens_remove(profanity) %>%
	tokens_ngrams(n = 5, concatenator = " ")

#Create results set from n-grams.
results <- data_frame(input = unlist(testQuintgrams)) %>%
	mutate(output = sub(".+ (.+)", "\\1", input), input = sub("(.+) .+", "\\1", input))

#results <- results %>% mutate(result = (predictWord(input) == output))
results <- results %>% mutate(result = system.time(predictWord(input))[1])
