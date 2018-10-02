# NLP-Project---Airlines
rm(list=ls())
library(tm)
library(DBI)
library(e1071)
library(maxent)
library(pROC)
library(RMySQL)
library(tidytext)

###############  Load Pos and Neg Lexicons  #######

afinn <- get_sentiments("afinn")
bing <- get_sentiments("bing")
nrc <- get_sentiments("nrc")
table(nrc$sentiment)

neg1 <- afinn$word[afinn$score < 0 ]
neg2 <- bing$word[bing$sentiment == "negative"]
neg3 <- nrc$sentiment[nrc$sentiment == "negative" | nrc$sentiment == "anger" |
                        nrc$sentiment == "disgust" | nrc$sentiment == "fear"|
                        nrc$sentiment == "sadness"]
neg.words <- c(neg1,neg2,neg3)

pos1 <- afinn$word[afinn$score > 0 ]
pos2 <- bing$word[bing$sentiment == "positive"]
pos3 <- nrc$sentiment[nrc$sentiment == "positive" | nrc$sentiment == "joy" | 
                        nrc$sentiment == "trust"]
pos.words <- c(pos1,pos2,pos3)

########### Build a Fuction of Sentiment Score  ##########################

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  #we got a vector of sentences. plyr will handle a list or a vector as an "l" for us 
  we want a simple array ("a") of scores back, 
  so we use "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

################# Output Sentiment Score  ########################

result <- score.sentiment(df$tweet,pos.words,neg.words)
summary(result$score)
hist(result$score,col ="yellow", main ="Score of tweets", ylab = " Count of tweets")
table(result$score)

##########I'm confident at a tweet with score >= 3 is non-complain#############

noncomplain <- result[result$score >= 3,]
noncomplain <- noncomplain[,-2]
write.csv(noncomplain, file = "Liangyu_HOU_1.csv")
