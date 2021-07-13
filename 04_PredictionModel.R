library(readr)
library(dplyr)
library(quanteda)

#Read in lookup tables.
lookupBigramTbl <- read_csv("./lookup_tables/lookupBigram.csv")
lookupBigram_s1Tbl <- read_csv("./lookup_tables/lookupBigram_s1.csv")
lookupQuadgramTbl <- read_csv("./lookup_tables/lookupQuadgram.csv")
lookupQuadgram_s1p2Tbl <- read_csv("./lookup_tables/lookupQuadgram_s1p2.csv")
lookupQuadgram_s1p3Tbl <- read_csv("./lookup_tables/lookupQuadgram_s1p3.csv")
lookupQuadgram_s1p4Tbl <- read_csv("./lookup_tables/lookupQuadgram_s1p4.csv")
lookupQuadgram_s2p23Tbl <- read_csv("./lookup_tables/lookupQuadgram_s2p23.csv")
lookupQuadgram_s2p24Tbl <- read_csv("./lookup_tables/lookupQuadgram_s2p24.csv")
lookupQuadgram_s2p34Tbl <- read_csv("./lookup_tables/lookupQuadgram_s2p34.csv")
lookupQuadgram_s3Tbl <- read_csv("./lookup_tables/lookupQuadgram_s3.csv")
lookupTrigramTbl <- read_csv("./lookup_tables/lookupTrigram.csv")
lookupTrigram_s1p2Tbl <- read_csv("./lookup_tables/lookupTrigram_s1p2.csv")
lookupTrigram_s1p3Tbl <- read_csv("./lookup_tables/lookupTrigram_s1p3.csv")
lookupTrigram_s2Tbl <- read_csv("./lookup_tables/lookupTrigram_s2.csv")
lookupUnigramTbl <- read_csv("./lookup_tables/lookupUnigram.csv")

lookupDefault <- read_lines("./lookup_tables/lookupDefault.txt")

#Create hash tables.
lookupBigram <- new.env(hash = TRUE)
lookupBigram_s1 <- new.env(hash = TRUE)
lookupQuadgram <- new.env(hash = TRUE)
lookupQuadgram_s1p2 <- new.env(hash = TRUE)
lookupQuadgram_s1p3 <- new.env(hash = TRUE)
lookupQuadgram_s1p4 <- new.env(hash = TRUE)
lookupQuadgram_s2p23 <- new.env(hash = TRUE)
lookupQuadgram_s2p24 <- new.env(hash = TRUE)
lookupQuadgram_s2p34 <- new.env(hash = TRUE)
lookupQuadgram_s3 <- new.env(hash = TRUE)
lookupTrigram <- new.env(hash = TRUE)
lookupTrigram_s1p2 <- new.env(hash = TRUE)
lookupTrigram_s1p3 <- new.env(hash = TRUE)
lookupTrigram_s2 <- new.env(hash = TRUE)
lookupUnigram <- new.env(hash = TRUE)

#Populate hash tables.
list2env(setNames(as.list(lookupBigramTbl[[2]]), lookupBigramTbl[[1]]), envir = lookupBigram)
list2env(setNames(as.list(lookupBigram_s1Tbl[[2]]), lookupBigram_s1Tbl[[1]]), envir = lookupBigram_s1)
list2env(setNames(as.list(lookupQuadgramTbl[[2]]), lookupQuadgramTbl[[1]]), envir = lookupQuadgram)
list2env(setNames(as.list(lookupQuadgram_s1p2Tbl[[2]]), lookupQuadgram_s1p2Tbl[[1]]), envir = lookupQuadgram_s1p2)
list2env(setNames(as.list(lookupQuadgram_s1p3Tbl[[2]]), lookupQuadgram_s1p3Tbl[[1]]), envir = lookupQuadgram_s1p3)
list2env(setNames(as.list(lookupQuadgram_s1p4Tbl[[2]]), lookupQuadgram_s1p4Tbl[[1]]), envir = lookupQuadgram_s1p4)
list2env(setNames(as.list(lookupQuadgram_s2p23Tbl[[2]]), lookupQuadgram_s2p23Tbl[[1]]), envir = lookupQuadgram_s2p23)
list2env(setNames(as.list(lookupQuadgram_s2p24Tbl[[2]]), lookupQuadgram_s2p24Tbl[[1]]), envir = lookupQuadgram_s2p24)
list2env(setNames(as.list(lookupQuadgram_s2p34Tbl[[2]]), lookupQuadgram_s2p34Tbl[[1]]), envir = lookupQuadgram_s2p34)
list2env(setNames(as.list(lookupQuadgram_s3Tbl[[2]]), lookupQuadgram_s3Tbl[[1]]), envir = lookupQuadgram_s3)
list2env(setNames(as.list(lookupTrigramTbl[[2]]), lookupTrigramTbl[[1]]), envir = lookupTrigram)
list2env(setNames(as.list(lookupTrigram_s1p2Tbl[[2]]), lookupTrigram_s1p2Tbl[[1]]), envir = lookupTrigram_s1p2)
list2env(setNames(as.list(lookupTrigram_s1p3Tbl[[2]]), lookupTrigram_s1p3Tbl[[1]]), envir = lookupTrigram_s1p3)
list2env(setNames(as.list(lookupTrigram_s2Tbl[[2]]), lookupTrigram_s2Tbl[[1]]), envir = lookupTrigram_s2)
list2env(setNames(as.list(lookupUnigramTbl[[2]]), lookupUnigramTbl[[1]]), envir = lookupUnigram)

#Cleanup unnecessary tables.
rm(lookupBigramTbl,
   lookupBigram_s1Tbl,
   lookupQuadgramTbl,
   lookupQuadgram_s1p2Tbl,
   lookupQuadgram_s1p3Tbl,
   lookupQuadgram_s1p4Tbl,
   lookupQuadgram_s2p23Tbl,
   lookupQuadgram_s2p24Tbl,
   lookupQuadgram_s2p34Tbl,
   lookupQuadgram_s3Tbl,
   lookupTrigramTbl,
   lookupTrigram_s1p2Tbl,
   lookupTrigram_s1p3Tbl,
   lookupTrigram_s2Tbl,
   lookupUnigramTbl)

#Create prediction function.
#Take at most the last 4 words and match it to the n-gram via lookup tables.
#Lookup tables used in order of precedence of the most words in closest proximity to the end.
predictWord <- function(s)
{
	#Tokenize input through quanteda in order to match formatting to lookup tables.
	x <- s %>% corpus() %>% corpus_reshape("sentence") %>% tail(1) %>% tokens(remove_punct = TRUE)
	x <- x[[1]] %>% tail(4)
	
	#Progress through lookup tables for a match.
	#Last 4 words.
	if(length(x) == 4)
	{
		key <- x %>% paste(collapse = "_")
		value <- lookupQuadgram[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#Last 3 words.
	if(length(x) >= 3)
	{
		key <- x %>% tail(3) %>% paste(collapse = "_")
		value <- lookupTrigram[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#3 words skipping 1.
	if(length(x) == 4)
	{
		#Skip 1 position 2.
		key <- c(x[1], x[3], x[4]) %>% paste(collapse = "_")
		value <- lookupQuadgram_s1p2[[key]]
		if(!is.null(value))
		{
			return(value)
		}
		
		#Skip 1 position 3.
		key <- c(x[1], x[2], x[4]) %>% paste(collapse = "_")
		value <- lookupQuadgram_s1p3[[key]]
		if(!is.null(value))
		{
			return(value)
		}
		
		#Skip 1 position 4.
		key <- c(x[1], x[2], x[3]) %>% paste(collapse = "_")
		value <- lookupQuadgram_s1p4[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#Last 2 words.
	if(length(x) >= 2)
	{
		key <- x %>% tail(2) %>% paste(collapse = "_")
		value <- lookupBigram[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#2 words skipping 1.
	if(length(x) >= 3)
	{
		#Skip 1 position 2.
		key <- x %>% tail(3)
		key <- c(key[1], key[3]) %>% paste(collapse = "_")
		value <- lookupTrigram_s1p2[[key]]
		if(!is.null(value))
		{
			return(value)
		}
		
		#Skip 1 position 3.
		key <- x %>% tail(3)
		key <- c(key[1], key[2]) %>% paste(collapse = "_")
		value <- lookupTrigram_s1p3[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#2 words skipping 2.
	if(length(x) == 4)
	{
		#Skip 2 position 2+3.
		key <- c(x[1], x[4]) %>% paste(collapse = "_")
		value <- lookupQuadgram_s2p23[[key]]
		if(!is.null(value))
		{
			return(value)
		}
		
		#Skip 2 position 2+4.
		key <- c(x[1], x[3]) %>% paste(collapse = "_")
		value <- lookupQuadgram_s2p24[[key]]
		if(!is.null(value))
		{
			return(value)
		}
		
		#Skip 2 position 3+4.
		key <- c(x[1], x[2]) %>% paste(collapse = "_")
		value <- lookupQuadgram_s2p34[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#Last 1 word.
	if(length(x) >= 1)
	{
		key <- x %>% tail(1)
		value <- lookupUnigram[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#Skipping 1.
	if(length(x) >= 2)
	{
		key <- x %>% tail(2)
		key <- key[1]
		value <- lookupBigram_s1[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#Skipping 2.
	if(length(x) >= 3)
	{
		key <- x %>% tail(3)
		key <- key[1]
		value <- lookupTrigram_s2[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#Skipping 3.
	if(length(x) == 4)
	{
		key <- x[1]
		value <- lookupQuadgram_s3[[key]]
		if(!is.null(value))
		{
			return(value)
		}
	}
	
	#No matches: return default.
	return(lookupDefault)
}