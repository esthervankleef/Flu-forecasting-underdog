################################
# ANALYSIS TWITTER DATA
################################

# Author: Esther van Kleef
# Date: 18 October
rm(list=ls())
library("twitteR")
library("tm")
library("wordcloud")

# Setting up tweet access
consumer_key <- "jPxzeXFg059734uo1EHwRVGg3"
consumer_secret <- "GT700RpaRCLOoKiyj2meYZcyjdhr7JkVPNM0PgcD87LJLzI9LQ"
access_token <- "788198243624595456-rexJcNhE2rL4tMs1ksuCQEcI4Bdqzlf"
access_secret <- "DQpkFL9ViEO68Scxhwyi33ALC0l7f01RuTyicgO1caMgT"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

flu <- searchTwitter("#Flu", n=1500)

#save text
flu_text <- sapply(flu, function(x) x$getText())

#create corpus
flu_text_corpus <- Corpus(VectorSource(flu_text))

#clean up
flu_text_corpus <- tm_map(flu_text_corpus,
                              content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                              mc.cores=1)
flu_text_corpus <- tm_map(flu_text_corpus, content_transformer(tolower), mc.cores=1)
flu_text_corpus <- tm_map(flu_text_corpus, removePunctuation, mc.cores=1)
flu_text_corpus <- tm_map(flu_text_corpus, function(x)removeWords(x,stopwords()), mc.cores=1)
wordcloud(flu_text_corpus,scale = c(4, 0.35)) 
