tweets <- searchTwitter("#hershey",n=15000,lang="en",since = '2016-03-31')
tweets_text <- sapply(tweets,function(x) x$getText())
object.size(tweets)
head(tweets_text)
tail(tweets_text)
#install.packages("SnowballC")
library(SnowballC)
tweets_text <- iconv(tweets_text,to="utf-8-mac")

#tweets_text <- iconv(tweets_text, to = "utf-8", sub="")
tweet_Corpus <- Corpus(VectorSource(tweets_text)) 
tweet_Corpus <- tm_map(tweet_Corpus,removeNumbers, lazy = 'TRUE')
tweet_Corpus <- tm_map(tweet_Corpus, stemDocument)   

tdm = TermDocumentMatrix(tweet_Corpus,
                         control = list(removePunctuation = TRUE,
                                        stopwords = c("hershey","chocolate","sweet","%http%","hersheys", "httpstcokepoqp",stopwords("english")),
                                        removeNumbers = TRUE, tolower = TRUE
                         ))
dtm <- DocumentTermMatrix(tweet_Corpus)   
dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   
inspect(dtms)

findFreqTerms(tdm)
# define tdm as matrix
m = as.matrix(tdm)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE) 
# create a data frame with words and their frequencies
dm = data.frame(word=names(word_freqs), freq=word_freqs)
# plot wordcloud
wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
findAssocs(dtm, c("marketing" , "job"), corlimit=0.5) # specifying a correlation limit of 0.98  
dtmss <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space, maximum.   

# To plot the most frequently occuring words
wf <- data.frame(word=names(word_freqs), freq=word_freqs)   
head(wf)
library(ggplot2)   
p <- ggplot(subset(wf, freq>100), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p 

