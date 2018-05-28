
extractTweets <- function(searchTerm,tweetCount){
  # search term tweets
  tweets = searchTwitter(searchTerm,n=tweetCount)
  tweets.df = twListToDF(tweets)
  tweets.df$text <- sapply(tweets.df$text,function(x)
    iconv(x,to='UTF-8'))
  return(tweets.df)
}


extractTimelineTweets <- function(username,tweetCount){
  # timeline tweets
  twitterUser <- getUser(username)
  tweets = userTimeline(twitterUser,n=tweetCount)
  tweets.df = twListToDF(tweets)
  tweets.df$text <- sapply(tweets.df$text,function(x) 
    iconv(x,to='UTF-8'))
  
  return(tweets.df)
}

transformTweets <- function(tweetDF){
  tweetCorpus <- Corpus(VectorSource(tweetDF$text))
  tweetCorpus <- tm_map(tweetCorpus, tolower)
  tweetCorpus <- tm_map(tweetCorpus, removePunctuation)
  tweetCorpus <- tm_map(tweetCorpus, removeNumbers)
  
  # remove URLs
  removeURL <- function(x) gsub("http://[[:alnum:]]*", "", x)
  tweetCorpus <- tm_map(tweetCorpus, removeURL) 
  
  # remove stop words
  twtrStopWords <- c(stopwords("english"),'rt','http','https')
  tweetCorpus <- tm_map(tweetCorpus, removeWords, twtrStopWords)
  
  #convert back to dataframe
  tweetDataframe<-data.frame(text=unlist(sapply(tweetCorpus, `[`, "content")), 
                             stringsAsFactors=F)
  
  #split each doc into words
  splitText <- function(x) {
    word.list = str_split(x, '\\s+')
    words = unlist(word.list)
  }
  
  tweetDataframe$wordList = sapply(tweetDataframe$text,function(text) splitText(text))
  
  return (tweetDataframe)
}

scoreTweet <- function(wordList) {
  # compare our words to the dictionaries of positive & negative terms
  pos.matches = match(wordList, pos.words)
  neg.matches = match(wordList, neg.words)
  
  # match() returns the position of the matched term or NA
  # we just want a TRUE/FALSE:
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  
  # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
  score = sum(pos.matches) - sum(neg.matches)
  return(score)
}

# Polarity analysis

analyzeTrendSentiments <- function(search,tweetCount){ 
  
  #extract tweets
  tweetsDF <- extractTweets(search,tweetCount)
  
  # transformations
  transformedTweetsDF <- transformTweets(tweetsDF)
  
  #score the words  
  transformedTweetsDF$sentiScore = sapply(transformedTweetsDF$wordList,
                                          function(wordList) scoreTweet(wordList))
  transformedTweetsDF$search <- search
  
  return(transformedTweetsDF) 
}

analyzeUserSentiments <- function(search,tweetCount){ 
  
  #extract tweets
  tweetsDF <- extractTimelineTweets(search,tweetCount) #extractTweets(search,tweetCount)
  
  # transformations
  transformedTweetsDF <- transformTweets(tweetsDF)
  
  #score the words  
  transformedTweetsDF$sentiScore = sapply(transformedTweetsDF$wordList,function(wordList) scoreTweet(wordList))
  transformedTweetsDF$search <- search
  
  return(transformedTweetsDF) 
}
library(twitteR)
library(stringr)
library(tm)
library(ggplot2)

consumerSecret = "Ehu9cOfJHhI5HdvbleBfT8ZxNdo5L5x1ykexWMDJYLcZOJXrDD"
consumerKey = "gbPunWvR5nccPLm7V4QVfvows"
accessToken = "999892351853084674-p2XSFUCwJLYmK0g9iDRmvEs9ixOMMFm"
accessTokenSecret = "JLUFbohRBbvPzabRS6Er9qOD8IqS9NkY5pfPLabbmXbBv"

setup_twitter_oauth(consumerKey, consumerSecret,
                    accessToken, accessTokenSecret)

pos.words = scan(file= 'positive-words.txt',
                 what='character', comment.char=';')
neg.words = scan(file= 'negative-words.txt', 
                 what='character', comment.char=';')

makeIndiaSentiments<- analyzeTrendSentiments("makeinindia",1500)

qplot(makeIndiaSentiments$sentiScore)

user1Timeline <- analyzeUserSentiments("narendramodi",1500)
user2Timeline <- analyzeUserSentiments("PMOIndia_RC",1500)
user3Timeline <- analyzeUserSentiments("PMOIndia",1500)

all.scores<- rbind(user1Timeline,user2Timeline,user3Timeline)

ggplot(data=all.scores)+geom_histogram(mapping=aes(x=sentiScore,fill=search),
                   binwidth = 1)+facet_grid(search~.)+scale_fill_grey()
                

library(e1071) 
library(caret) 
library(kernlab) 
library(ROCR) 
library(RTextTools)
source("performance_plot_utils.R")

labeledDSFilePath = "labeled_tweets.csv"
labeledDataset = read.csv(labeledDSFilePath, header = FALSE)

labeledDataset$V1 = sapply(labeledDataset$V1,
                           function(x)
                             if(x==4)
                               x <- "positive"
                           else if(x==0)
                             x<-"negative"
                           else x<- "none")

requiredColumns<-c("V1","V6")

tweets<-as.matrix(labeledDataset[labeledDataset$V1 
                                 %in% c("positive","negative"),
                                 requiredColumns])

indexes <- createDataPartition(tweets[,1], p=0.7, list = FALSE)

train.data <- tweets[indexes,]
test.data <- tweets[-indexes,]

prop.table(table(tweets[,1]))

prop.table(table(tweets[indexes,1]))

prop.table(table(tweets[-indexes,1]))

train.dtMatrix <- create_matrix(train.data[,2], 
                                language="english" , 
                                removeStopwords=TRUE, 
                                removeNumbers=TRUE,
                                stemWords=TRUE,
                                weighting = tm::weightTfIdf)

test.dtMatrix <- create_matrix(test.data[,2], 
                               language="english" , 
                               removeStopwords=TRUE, 
                               removeNumbers=TRUE,
                               stemWords=TRUE,
                               weighting = tm::weightTfIdf,
                               originalMatrix=train.dtMatrix)

test.data.size<- nrow(test.data)


svm.model<- svm(train.dtMatrix, as.factor(train.data[,1]))

summary(svm.model)

svm.predictions<- predict(svm.model, test.dtMatrix)

true.labels<- as.factor(test.data[,1])

confusionMatrix(data=svm.predictions, reference=true.labels,
                positive="positive")

cost.weights <- c(0.1, 10, 100)
gamma.weights <- c(0.01, 0.25, 0.5, 1)
tuning.results <- tune(svm, train.dtMatrix,as.factor(train.data[,1]),
                       kernel="radial", 
                       ranges=list(cost=cost.weights, gamma=gamma.weights))

print(tuning.results)

plot(tuning.results, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")

svm.model.best = tuning.results$best.model
svm.predictions.best <- predict(svm.model.best, test.dtMatrix)
confusionMatrix(data=svm.predictions.best, reference=true.labels,
                positive="positive")
svm.predictions.best <- predict(svm.model.best,
                                test.dtMatrix, decision.values = T)

svm.prediction.values <- attributes(svm.predictions.best)$decision.values

predictions <- prediction(svm.prediction.values, true.labels)

par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="SVM ROC Curve")
plot.pr.curve(predictions, title.text="SVM Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

# Sentiment Classification of tweets using Boosting

train.container <- create_container(train.dtMatrix, 
                                    as.factor(train.data[,1]), 
                                    trainSize=1:nrow(train.data), 
                                    virgin=FALSE)

test.container <- create_container(test.dtMatrix, 
                                   labels=rep(0,test.data.size), 
                                   testSize=1:test.data.size, 
                                   virgin=FALSE)

boosting.model<- train_model(train.container,"BOOSTING",
                             maxitboost = 500)

boosting.classify<-classify_model(test.container, boosting.model)

predicted.labels <- boosting.classify[,1]
true.labels <- as.factor(test.data[,1])

confusionMatrix(data = predicted.labels, 
                reference = true.labels, 
                positive = "positive")

predictions <- prediction(boosting.classify[,"LOGITBOOST_PROB"],
                true.labels,label.ordering=c("negative","positive"))

par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="Boosting ROC Curve")
plot.pr.curve(predictions, title.text="Boosting Precision/Recall Curve")

auc<- performance(predictions,"auc")
auc<- unlist(slot(auc,"y.values"))
auc<- round(auc,4)
auc

N=10

set.seed(42)

cross_validate(train.container,N,"BOOSTING", maxitboost = 500)




