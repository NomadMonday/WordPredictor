library(dplyr)
library(readr)
library(quanteda)

#Read in the text file data sources.
textBlogs <- read_lines("./datasets/TrainBlogs.txt")
textNews <- read_lines("./datasets/TrainNews.txt")
textTwitter <- read_lines("./datasets/TrainTwitter.txt")

#Convert to data frames for tidying.
dfBlogs <- data_frame(line = paste("blogs", 1:length(textBlogs)), text = textBlogs)
dfNews <- data_frame(line = paste("news", 1:length(textNews)), text = textNews)
dfTwitter <- data_frame(line = paste("twitter", 1:length(textTwitter)), text = textTwitter)

rm(textBlogs, textNews, textTwitter)

#Profanity filter resource: https://github.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
profanity <- read_lines("./en_profanity.txt")

#Create small sample for initial use.
set.seed(1235)
sampleSize <- .5

sampleBlogs <- dfBlogs %>% sample_frac(size = sampleSize)
sampleNews <- dfNews %>% sample_frac(size = sampleSize)
sampleTwitter <- dfTwitter %>% sample_frac(size = sampleSize)

dfCombined <- bind_rows(sampleBlogs, sampleNews, sampleTwitter)

rm(sampleBlogs, sampleNews, sampleTwitter, dfBlogs, dfNews, dfTwitter)

#Create corpus and tokenize, removing profanity matches.
corp <- corpus(dfCombined)
docnames(corp) <- dfCombined$line
sentenceCorp <- corp %>% corpus_reshape("sentences")
toks <- sentenceCorp %>% tokens(remove_punct = TRUE)
toks <- toks %>% tokens_remove(profanity)

rm(dfCombined, profanity, corp, sentenceCorp)

#Create n-gram lookup tables.
#Create a data_frame with the feature counts. Extract the first n-1 words as the related n-1gram.
#Take the highest occuring ngram for each n-1gram. Extract the last word as the prediction.
#Write to file and clean up.
#Also create variations with certain word positions skipped.

quintgramToks <- toks %>% tokens_ngrams(n = 5)

quintgrams <- quintgramToks %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+_.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram, "./lookup_tables/lookupQuadgram.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\3\\4\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s1p2 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s1p2, "./lookup_tables/lookupQuadgram_s1p2.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\2\\4\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s1p3 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s1p3, "./lookup_tables/lookupQuadgram_s1p3.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\2\\3\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s1p4 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s1p4, "./lookup_tables/lookupQuadgram_s1p4.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\4\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s2p23 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s2p23, "./lookup_tables/lookupQuadgram_s2p23.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\3\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s2p24 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s2p24, "./lookup_tables/lookupQuadgram_s2p24.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\2\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s2p34 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+_.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s2p34, "./lookup_tables/lookupQuadgram_s2p34.csv")

quintgrams <- quintgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+_)(.+)", "\\1\\5", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupQuadgram_s3 <- data_frame(quintgram = featnames(quintgrams), counts = as.integer(colSums(quintgrams))) %>%
	mutate(quadgram = sub("(.+)_.+", "\\1", quintgram)) %>% group_by(quadgram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quintgram)) %>%
	select(quadgram, prediction)
write_csv(lookupQuadgram_s3, "./lookup_tables/lookupQuadgram_s3.csv")

rm(quintgrams, quintgramToks)


quadgramToks <- toks %>% tokens_ngrams(n = 4)

quadgrams <- quadgramToks %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupTrigram <- data_frame(quadgram = featnames(quadgrams), counts = as.integer(colSums(quadgrams))) %>%
	mutate(trigram = sub("(.+_.+_.+)_.+", "\\1", quadgram)) %>% group_by(trigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quadgram)) %>%
	select(trigram, prediction)
write_csv(lookupTrigram, "./lookup_tables/lookupTrigram.csv")

quadgrams <- quadgramToks %>%
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+)", "\\1\\3\\4", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupTrigram_s1p2 <- data_frame(quadgram = featnames(quadgrams), counts = as.integer(colSums(quadgrams))) %>%
	mutate(trigram = sub("(.+_.+)_.+", "\\1", quadgram)) %>% group_by(trigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quadgram)) %>%
	select(trigram, prediction)
write_csv(lookupTrigram_s1p2, "./lookup_tables/lookupTrigram_s1p2.csv")

quadgrams <- quadgramToks %>%
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+)", "\\1\\2\\4", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupTrigram_s1p3 <- data_frame(quadgram = featnames(quadgrams), counts = as.integer(colSums(quadgrams))) %>%
	mutate(trigram = sub("(.+_.+)_.+", "\\1", quadgram)) %>% group_by(trigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quadgram)) %>%
	select(trigram, prediction)
write_csv(lookupTrigram_s1p3, "./lookup_tables/lookupTrigram_s1p3.csv")

quadgrams <- quadgramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+_)(.+)", "\\1\\4", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupTrigram_s2 <- data_frame(quadgram = featnames(quadgrams), counts = as.integer(colSums(quadgrams))) %>%
	mutate(trigram = sub("(.+)_.+", "\\1", quadgram)) %>% group_by(trigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", quadgram)) %>%
	select(trigram, prediction)
write_csv(lookupTrigram_s2, "./lookup_tables/lookupTrigram_s2.csv")

rm(quadgrams, quadgramToks)


trigramToks <- toks %>% tokens_ngrams(n = 3)

trigrams <- trigramToks %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupBigram <- data_frame(trigram = featnames(trigrams), counts = as.integer(colSums(trigrams))) %>%
	mutate(bigram = sub("(.+_.+)_.+", "\\1", trigram)) %>% group_by(bigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", trigram)) %>%
	select(bigram, prediction)
write_csv(lookupBigram, "./lookup_tables/lookupBigram.csv")

trigrams <- trigramToks %>% 
	lapply(function(y){sub("(.+_)(.+_)(.+)", "\\1\\3", y)}) %>%
	as.tokens() %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupBigram_s1 <- data_frame(trigram = featnames(trigrams), counts = as.integer(colSums(trigrams))) %>%
	mutate(bigram = sub("(.+)_.+", "\\1", trigram)) %>% group_by(bigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", trigram)) %>%
	select(bigram, prediction)
write_csv(lookupBigram_s1, "./lookup_tables/lookupBigram_s1.csv")

rm(trigrams, trigramToks)


bigrams <- toks %>% tokens_ngrams(n = 2) %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupUnigram <- data_frame(bigram = featnames(bigrams), counts = as.integer(colSums(bigrams))) %>%
	mutate(unigram = sub("(.+)_.+", "\\1", bigram)) %>% group_by(unigram) %>%
	slice(which.max(counts)) %>%
	mutate(prediction = sub(".+_(.+)", "\\1", bigram)) %>%
	select(unigram, prediction)
write_csv(lookupUnigram, "./lookup_tables/lookupUnigram.csv")

rm(bigrams)

unigrams <- toks %>% dfm() %>% dfm_trim(min_termfreq = 2)
lookupDefault <- unigrams %>% topfeatures(1) %>% names()
write(lookupDefault, "./lookup_tables/lookupDefault.txt")

rm(unigrams)

#Test object sizes
format(object.size(lookupQuadgram), units = "Mb")
format(object.size(lookupTrigram), units = "Mb")
format(object.size(lookupBigram), units = "Mb")
format(object.size(lookupUnigram), units = "Mb")
format(object.size(lookupQuadgram_s1p2), units = "Mb")
format(object.size(lookupQuadgram_s1p3), units = "Mb")
format(object.size(lookupQuadgram_s1p4), units = "Mb")
format(object.size(lookupQuadgram_s2p23), units = "Mb")
format(object.size(lookupQuadgram_s2p24), units = "Mb")
format(object.size(lookupQuadgram_s2p34), units = "Mb")
format(object.size(lookupQuadgram_s3), units = "Mb")
format(object.size(lookupTrigram_s1p2), units = "Mb")
format(object.size(lookupTrigram_s1p3), units = "Mb")
format(object.size(lookupTrigram_s2), units = "Mb")
format(object.size(lookupBigram_s1), units = "Mb")

format(object.size(lookupQuadgram) +
	   	object.size(lookupTrigram) +
	   	object.size(lookupBigram) +
	   	object.size(lookupUnigram) +
	   	object.size(lookupQuadgram_s1p2) +
	   	object.size(lookupQuadgram_s1p3) +
	   	object.size(lookupQuadgram_s1p4) +
	   	object.size(lookupQuadgram_s2p23) +
	   	object.size(lookupQuadgram_s2p24) +
	   	object.size(lookupQuadgram_s2p34) +
	   	object.size(lookupQuadgram_s3) +
	   	object.size(lookupTrigram_s1p2) +
	   	object.size(lookupTrigram_s1p3) +
	   	object.size(lookupTrigram_s2) +
	   	object.size(lookupBigram_s1)
	   , units = "Mb")
