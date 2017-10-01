#Text mining on tweets

#Install and load the TwitteR package
#install.packages("twitteR")
library(twitteR)
library(devtools)

#Install and load the Package tm (text mining package)
#install.packages("tm")
library(tm)

#Twitter authentication
api_key <- "aOxBF1tIWPjxGKnltr78CcqQd"
api_secret <- "fPn2CDWdvizRKHfitsYUWddtEw0nS1cPCO7m4Ut7lInQgVI7os"
access_token <- "4716529993-hPyLk0KXyJHbdnv6TgVuJY1yn1l6fvmm3lXOrAM"
access_token_secret <- "6irlrRfNdSVK3wcsgyuE4ghiWXdQxH9Gwi1ihhlq0ByRy"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) # authentification

#Donald Trump tweets analysis
tweets_Trump <- userTimeline("realDonaldTrump", n = 3200) #Get tweets from Donald Trump account
n.tweets_Trump <- length(tweets_Trump) #Number of tweets
tweets_Trump[[1]] #Display latest tweet

df.Trump <- twListToDF(tweets_Trump) #Transform tweets in a dataframe
dim(df.Trump) #Get dimension of the dataframe
df.Trump$text[1] #Display just text in dataframe, last tweet

Vector.Source<-VectorSource(df.Trump$text) #VectorSource: interprets each element of the vector x as a document.
names(Vector.Source)
Vector.Source$content[2]
myCorpus_Trump<-Corpus(VectorSource(df.Trump$text)) #Corpus: representing and computing on corpora

as.character(myCorpus_Trump[[2]])
myCorpus_Trump <- tm_map(myCorpus_Trump, function(x) iconv(enc2utf8(x), sub = "byte"))
myCorpus_Trump <- tm_map(myCorpus_Trump, content_transformer(tolower)) #Transform to text to lower case
as.character(myCorpus_Trump[[2]])

#Remove URLs function
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus_Trump <- tm_map(myCorpus_Trump,content_transformer(removeURL))
as.character(myCorpus_Trump[[2]])

#Remove stop words
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus_Trump <- tm_map(myCorpus_Trump,content_transformer(removeNumPunct))
as.character(myCorpus_Trump[[2]])

length(stopwords('english'))
myStopwords.english <- c(setdiff(stopwords('english'), c("very", "big")),"use", "see", "used", "via", "amp")
length(myStopwords.english)

myCorpus_Trump[[2]]$content
myCorpus_Trump <- tm_map(myCorpus_Trump, removeWords,myStopwords.english)
myCorpus_Trump[[2]]$content

myCorpus_Trump <- tm_map(myCorpus_Trump, stripWhitespace)
myCorpus_Trump[[2]]$content

myCorpus_Trump.Copy <- myCorpus_Trump #copy of the corpors to be used as dictionary

as.character(myCorpus_Trump.Copy[[2]])
myCorpus_Trump.Copy <- tm_map(myCorpus_Trump.Copy,stemDocument) #stemming process
as.character(myCorpus_Trump.Copy[[2]])

stemCompletion2 <- function(x, dictionary){
  x <- unlist(strsplit(as.character(x), " ")) 
  x <- x[x != ""] 
  x <- stemCompletion(x, dictionary=dictionary) 
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

myCorpus_Trump.Copy <- lapply(myCorpus_Trump.Copy, stemCompletion2,dictionary=myCorpus_Trump)
as.character(myCorpus_Trump.Copy[[2]])
myCorpus_Trump.Copy <- Corpus(VectorSource(myCorpus_Trump.Copy))
as.character(myCorpus_Trump.Copy[[2]])

# count word frequence
wordFreq <- function(corpus, word){
  results <- lapply(corpus,function(x) { grep(as.character(x), pattern=paste0("\\<",word)) })
  sum(unlist(results))
}

n.clinton <- wordFreq(myCorpus_Trump.Copy, "clinton")
n.clinton
length(myCorpus_Trump.Copy)

# replace oldword with newword
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
  pattern=oldword, replacement=newword)
}


#Term-Document matrix
tdm <- TermDocumentMatrix(myCorpus_Trump.Copy, control = list(wordLengths = c(1, Inf)))
labels(dimnames(tdm))

idx <- which(dimnames(tdm)$Terms == "clinton")
inspect(tdm[idx + (0:5), 101:110])

freq.terms <- findFreqTerms(tdm, lowfreq = 20)
freq.terms
