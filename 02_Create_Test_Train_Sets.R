#Split source data into testing, training, and validation sets.
library(readr)
library(dplyr)
library(caret)
set.seed(2956)

#Read in the text file data sources.
#https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
textBlogs <- read_lines("./final/en_US/en_US.blogs.txt")
textNews <- read_lines("./final/en_US/en_US.news.txt")
textTwitter <- read_lines("./final/en_US/en_US.twitter.txt")

#Separate into training, testing, and validation sets. 60/20/20
seqSet <- seq_along(textBlogs)
inTrainBlogs <- sample(seqSet, size = floor(.6 * length(seqSet)))
inTestBlogs <- sample(seqSet[-inTrainBlogs], size = floor(.5 * length(seqSet[-inTrainBlogs])))
trainBlogs <- textBlogs[inTrainBlogs]
testBlogs <- textBlogs[inTestBlogs]
validBlogs <- textBlogs[-c(inTrainBlogs, inTestBlogs)]

seqSet <- seq_along(textNews)
inTrainNews <- sample(seqSet, size = floor(.6 * length(seqSet)))
inTestNews <- sample(seqSet[-inTrainNews], size = floor(.5 * length(seqSet[-inTrainNews])))
trainNews <- textNews[inTrainNews]
testNews <- textNews[inTestNews]
validNews <- textNews[-c(inTrainNews, inTestNews)]

seqSet <- seq_along(textTwitter)
inTrainTwitter <- sample(seqSet, size = floor(.6 * length(seqSet)))
inTestTwitter <- sample(seqSet[-inTrainTwitter], size = floor(.5 * length(seqSet[-inTrainTwitter])))
trainTwitter <- textTwitter[inTrainTwitter]
testTwitter <- textTwitter[inTestTwitter]
validTwitter <- textTwitter[-c(inTrainTwitter, inTestTwitter)]

#Write sets to new files.
write_lines(trainBlogs, "./datasets/TrainBlogs.txt")
write_lines(testBlogs, "./datasets/TestBlogs.txt")
write_lines(validBlogs, "./datasets/ValidationBlogs.txt")

write_lines(trainNews, "./datasets/TrainNews.txt")
write_lines(testNews, "./datasets/TestNews.txt")
write_lines(validNews, "./datasets/ValidationNews.txt")

write_lines(trainTwitter, "./datasets/TrainTwitter.txt")
write_lines(testTwitter, "./datasets/TestTwitter.txt")
write_lines(validTwitter, "./datasets/ValidationTwitter.txt")