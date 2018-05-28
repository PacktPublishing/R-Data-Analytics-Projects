## Social Media Analysis - Analyzing Twitter Data

library(twitteR)

consumerSecret = "Ehu9cOfJHhI5HdvbleBfT8ZxNdo5L5x1ykexWMDJYLcZOJXrDD"
consumerKey = "gbPunWvR5nccPLm7V4QVfvows"
accessToken = "999892351853084674-p2XSFUCwJLYmK0g9iDRmvEs9ixOMMFm"
accessTokenSecret = "JLUFbohRBbvPzabRS6Er9qOD8IqS9NkY5pfPLabbmXbBv"

setup_twitter_oauth(consumerKey, consumerSecret,
                    accessToken, accessTokenSecret)

twitterUser<- getUser("jack")

tweets <- userTimeline(twitterUser, n=300)

tweets

tweets[[1]]$getClass()

tweets[[1]]$retweetCount

tweets[[1]]$favoriteCount

## Twitter data mining

library(ggplot2)
library(stringr)
library(tm)
library(wordcloud)

trendingTweets = searchTwitter("#Resolutions",n=1000)
trendingTweets.df = twListToDF(trendingTweets)
trendingTweets.df$text <- sapply(trendingTweets.df$text,
                                 function(x) iconv(x,to='UTF-8'))

enodeSource <- function(x) {
  if(x=="<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>"){
    gsub("<a href=\"http://twitter.com/download/iphone\" rel=\"nofollow\">Twitter for iPhone</a>", "iPhone", x,fixed=TRUE)
  }else if(x=="<a href=\"http://twitter.com/#!/download/ipad\" rel=\"nofollow\">Twitter for iPad</a>"){
    gsub("<a href=\"http://twitter.com/#!/download/ipad\" rel=\"nofollow\">Twitter for iPad</a>","iPad",x,fixed=TRUE)
  }else if(x=="<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>"){
    gsub("<a href=\"http://twitter.com/download/android\" rel=\"nofollow\">Twitter for Android</a>","Android",x,fixed=TRUE)
  } else if(x=="<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>"){
    gsub("<a href=\"http://twitter.com\" rel=\"nofollow\">Twitter Web Client</a>","Web",x,fixed=TRUE)
  } else if(x=="<a href=\"http://www.twitter.com\" rel=\"nofollow\">Twitter for Windows Phone</a>"){
    gsub("<a href=\"http://www.twitter.com\" rel=\"nofollow\">Twitter for Windows Phone</a>","Windows Phone",x,fixed=TRUE)
  }else {
    x
  }
}

trendingTweets.df$tweetSource = sapply(trendingTweets.df$statusSource,
                      function(sourceSystem) enodeSource(sourceSystem))

tweetCorpus<- Corpus(VectorSource(trendingTweets.df$text))

tweetCorpus <- tm_map(tweetCorpus, tolower)
tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
tweetCorpus <- tm_map(tweetCorpus, removeNumbers)

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
tweetCorpus <- tm_map(tweetCorpus, removeURL)

twtrStopWords <- c(stopwords("english"),'resolution','resolutions',
                   'resolutionsfor','resolutionsfor2016','2018',
                   'new','year','years','newyearresolution')
tweetCorpus <- tm_map(tweetCorpus, removeWords, twtrStopWords)

twtrTermDocMatrix<- TermDocumentMatrix(tweetCorpus,
                                       control = list(minWordLength=1))

which(apply(twtrTermDocMatrix,1,sum)>=15)

(frequentTerms<- findFreqTerms(twtrTermDocMatrix, lowfreq = 10))

term.freq<- rowSums(as.matrix(twtrTermDocMatrix))

subsetterm.freq <- subset(term.freq, term.freq>=10)

frequentTermsSubsetDF<- data.frame( term = names(subsetterm.freq),
                                    freq= subsetterm.freq)

frequentTermsDF<- data.frame(term= names(term.freq),
                             freq= term.freq)

frequentTermsSubsetDF <- frequentTermsSubsetDF[with(frequentTermsSubsetDF,
                                order(-frequentTermsSubsetDF$freq)), ]
frequentTermsDF <- frequentTermsDF[with(frequentTermsDF, 
                                        order(-frequentTermsDF$freq)), ]

ggplot(frequentTermsSubsetDF, aes(x = reorder(term,freq), y = freq)) + 
  geom_bar(stat = "identity") +xlab("Terms") +
  ylab("Frequency") + coord_flip()

wordcloud(words= frequentTermsDF$term, freq = frequentTermsDF$freq,
          random.order = FALSE)

head(subset(trendingTweets.df$text, grepl("living", 
                                      trendingTweets.df$text)), n=1)

(fitness.associations<- findAssocs(twtrTermDocMatrix,"health",0.25))

fitnessTerm.freq <- rowSums(as.matrix(fitness.associations$health))
fitnessDF <- data.frame(term=names(fitnessTerm.freq),
                        freq=fitnessTerm.freq)
fitnessDF <- fitnessDF[with(fitnessDF, order(-fitnessDF$freq)), ]

ggplot(fitnessDF,aes(x=reorder(term,freq),y=freq))+
  geom_bar(stat = "identity") +xlab("Terms") +
  ylab("Associations") + coord_flip()

##Popular Devices

colnames(trendingTweets.df)

trendingTweetsSubset.df<- subset(trendingTweets.df,
                                 trendingTweets.df$retweetCount>=4)
  
ggplot(trendingTweetsSubset.df,aes(x = tweetSource,y=retweetCount/100))+
  geom_bar(stat = "identity") +xlab("Source") + ylab("Retweet Count") 

## Hierarchical clustering

twtrTermDocMatrix2<- removeSparseTerms(twtrTermDocMatrix,sparse = 0.98)

tweet_matrix<- as.matrix(twtrTermDocMatrix2)

distMatrix<- dist(scale(tweet_matrix))

fit<- hclust(distMatrix, method = "single")
plot(fit)

atISS<- getUser("ISS_Research")

tweets<- userTimeline(atISS, n=1000)

tweets.df=twListToDF(tweets)

tweets.df$text<- sapply(tweets.df$text, function(x) iconv(x,to='UTF-8'))

#transformations
twtrCorpus <- Corpus(VectorSource(tweets.df$text))
twtrCorpus <- tm_map(twtrCorpus, tolower)
twtrCorpus <- tm_map(twtrCorpus, removePunctuation)
twtrCorpus <- tm_map(twtrCorpus, removeNumbers)
myStopwords <- c(stopwords("english"), "available", "via","amp",
                 "space","outerspace","spacestation","issresearch",
                 "nasa","science")
twtrCorpus <- tm_map(twtrCorpus, removeWords, myStopwords)

twtrDTM<- DocumentTermMatrix(twtrCorpus, control= list(minWordLength=1))

ldaTopics<- LDA(twtrDTM, k=8)

ldaTerms <- terms(ldaTopics,6)

(ldaTerms<- apply(ldaTerms, MARGIN = 2,paste, collapse=", "))

firstTopic<- topics(ldaTopics, 1)

topics <- data.frame(date=as.Date(tweets.df$created), firstTopic)
qplot(date, ..count.., data=topics, geom="density",
      fill=ldaTerms[firstTopic], position="stack")+scale_fill_grey()

qplot(date, ..count.., data=topics, geom="density",
      fill=ldaTerms[firstTopic], position="stack")

