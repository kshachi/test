
# Install necessary packages for twitter text mining.

install.packages("twitteR")
install.packages("RCurl")
library(twitteR)
library(RCurl)

# Set up authorization for Twitter API
consumer_key <- 'xxxx'
consumer_secret <- 'xxxx'
access_token <- 'xxxx'
access_token_secret <- 'xxxx'
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_token_secret)

# Search twitter for the hashtag Hershey from 31st March. Fetch 150 tweets.
tweets <- searchTwitter("#hershey",n=150,lang="en",since = '2016-03-31')

# Find the size of the tweets vector
object.size(tweets)

# Convert tweets to text.
tweets_text <- sapply(tweets,function(x) x$getText())

# Display the first and last few tweets.
head(tweets_text)
tail(tweets_text)

# Install text mining packages
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
install.packages("wordcloud")
library(wordcloud)
install.packages("RColorBrewer")
library(RColorBrewer)
tweets_text <- iconv(tweets_text,to="utf-8-mac")

# Create a corpus of tweets extracted.
tweet_Corpus <- Corpus(VectorSource(tweets_text)) 
tweet_Corpus <- tm_map(tweet_Corpus,removeNumbers, lazy = 'TRUE')
tweet_Corpus <- tm_map(tweet_Corpus, stemDocument)  

# Store the corpus in the form of a term document matrix.
tdm = TermDocumentMatrix(tweet_Corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("hershey","chocolate","sweet","%http%","hersheys", "httpstcokepoqp",stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE
                         ))

# define tdm as matrix
m = as.matrix(tdm)

# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 

# To plot the most frequently occuring words
wf <- data.frame(word=names(word_freqs), freq=word_freqs)   
head(wf)

# Install package required to plot data.
install.packages("ggplot2")
library(ggplot2)   
p <- ggplot(subset(wf, freq>100), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 
