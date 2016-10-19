################################
# TWITTER DATA EXPERIMENT
################################
# Turns out that only tweets up to 7 days old can be collected when using the standard API tool with R
# I have seen references to Python based packages, e.g. Tweepy

# Author: Esther van Kleef
# Date: 18 October
rm(list=ls())
library("twitteR")
library("tm")
library("wordcloud"); library(ggplot2)
library(topicmodels)

# Setting up tweet access
consumer_key <- "jPxzeXFg059734uo1EHwRVGg3"
consumer_secret <- "GT700RpaRCLOoKiyj2meYZcyjdhr7JkVPNM0PgcD87LJLzI9LQ"
access_token <- "788198243624595456-rexJcNhE2rL4tMs1ksuCQEcI4Bdqzlf"
access_secret <- "DQpkFL9ViEO68Scxhwyi33ALC0l7f01RuTyicgO1caMgT"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

flu <- searchTwitter("flu AND I -vaccine -bird -shot", lang="en", geocode="39.6492797,-98.220699,1800mi",n=1000, since="2006-01-01")

# Transfer to DF
flu.df <- twListToDF(flu)

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
#wordcloud(flu_text_corpus,scale = c(4, 0.35)) 

# Count frequencies of words
tdm <- TermDocumentMatrix(flu_text_corpus,control = list(wordLengths = c(1, Inf))) # so we can use findFreqTerms
freq.terms <- findFreqTerms(tdm, lowfreq = 20) # Find frequency of words used
freq.terms

term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 100) # select those which are used 100 times at minimum

df <- data.frame(term = names(term.freq), freq = term.freq)

# Plot most frequent terms used
ggplot(df, aes(y=freq, x=reorder(term,freq,
                             function(x)-max(x)))) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip() + theme(axis.text=element_text(size=7))

# Find associated between terms and one other term
findAssocs(tdm, "influenza", 0.2)

# TOPIC MODELLING
dtm <- as.DocumentTermMatrix(tdm)
lda <- LDA(dtm, k = 8) # find 8 topics
term <- terms(lda, 7) # first 7 terms of every topic

term <- apply(term, MARGIN = 2, paste, collapse = ", ")

topics <- topics(lda) # 1st topic identified for every document (tweet)
topics <- data.frame(date=as.Date(flu.df$created), topic=topics)

ggplot(topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")
