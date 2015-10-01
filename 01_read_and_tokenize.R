rm(list=ls())
library(tm)
library(RCurl)
library(LaF)
library(SnowballC)
library(RWeka)
library(slam)
library(data.table)
startwd<-getwd()
setwd("~/Dropbox/DSL/Coursera Data Science/Coursework/capstone/final/en_US/")
#Make file names
blogs<-"en_US.blogs.txt"
news<-"en_US.news.txt"
twitter<-"en_US.twitter.txt"

set.seed(1999)
#count number of lines in each file
b<-determine_nlines("en_US.blogs.txt")
n<-determine_nlines("en_US.news.txt")
t<-determine_nlines("en_US.twitter.txt")
#randomly sample 1/10 of the lines in each file

#blograw<-paste(sample_lines(blogs,b/50,b), collapse = " ")
blograw<-sample_lines(blogs,b/18,b)

#newsraw<-paste(sample_lines(news,n/50,n),collapse = " ")
newsraw<-sample_lines(news,n/18,n)

#twitterraw<-paste(sample_lines(twitter,t/50,t),collapse = " ")
twitterraw<-sample_lines(twitter,t/18,t)

##CLEANING THE DATA

# read in profanity from shuttershock github https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
profanity<-read.table(text=getURL("https://raw.githubusercontent.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"),
                      sep="/")

profanity<-as.vector(profanity$V1)
#now make a function to clean each set of texts
clean_text<-function(text){
          
          #convert text into corpus object 
          texttf<-Corpus(VectorSource(text))
          #put everything in lower case
          texttf<-tm_map(texttf,content_transformer(tolower))
          #convert everything to stems
          #texttf<-tm_map(texttf,stemDocument)
          #now punctuation, numbers
          texttf<-tm_map(texttf,content_transformer(removePunctuation))
          #texttf<-tm_map(texttf,content_transformer(removeNumbers))
          #and get rid of stop words and profanity
          #texttf<-tm_map(texttf,removeWords,stopwords("en"))
          texttf<-tm_map(texttf,removeWords,profanity)
          
          #now extra white space created after removing so much stuff
          texttf<-tm_map(texttf,stripWhitespace)
          return(texttf)
}

blogcorpus<-clean_text(blograw)
twittercorpus<-clean_text(twitterraw)
newscorpus<-clean_text(newsraw)

rm(blogs,news,twitter,blograw,twitterraw,newsraw,b,t,n,profanity) #conserve memory 


options(mc.cores=1)
#UniigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))

#blogdtm <- TermDocumentMatrix(blogcorpus, control = list(tokenize = UniigramTokenizer))
#newsdtm<-TermDocumentMatrix(newscorpus, control = list(tokenize = UniigramTokenizer))
#twitterdtm<-TermDocumentMatrix(twittercorpus, control = list(tokenize = UniigramTokenizer))

#blog_tokens<-rowSums(as.matrix(rollup(blogdtm,2,na.rm=TRUE,FUN=sum)))
#news_tokens<-rowSums(as.matrix(rollup(newsdtm,2,na.rm=TRUE,FUN=sum)))
#twitter_tokens<-rowSums(as.matrix(rollup(twitterdtm,2,na.rm=TRUE,FUN=sum)))


#count vectorize unigrams
#top_twitter<-sort(x = twitter_tokens,decreasing = TRUE)[1:10]
#top_blog<-sort(x = blog_tokens,decreasing = TRUE)[1:10]
#top_news<-sort(x = news_tokens,decreasing = TRUE)[1:10]

##################
######Bigrams####

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
blogdtm2 <- TermDocumentMatrix(blogcorpus, control = list(tokenize = BigramTokenizer))
newsdtm2<-TermDocumentMatrix(newscorpus, control = list(tokenize = BigramTokenizer))
twitterdtm2<-TermDocumentMatrix(twittercorpus, control = list(tokenize = BigramTokenizer))

sbt=strsplit(c(blogdtm2$dimnames$Terms,
                              newsdtm2$dimnames$Terms,
                              twitterdtm2$dimnames$Terms),split=" ")
df2=data.frame(t(do.call(cbind, sbt)))
for (i in 1:ncol(df2)){df2[,i]<-as.character(df2[,i])}
#df2$string<-df2$X1


#blog_tokens2<-rowSums(as.matrix(rollup(blogdtm2,2,na.rm=TRUE,FUN=sum)))
#news_tokens2<-rowSums(as.matrix(rollup(newsdtm2,2,na.rm=TRUE,FUN=sum)))
#twitter_tokens2<-rowSums(as.matrix(rollup(twitterdtm2,2,na.rm=TRUE,FUN=sum)))

#top_twitter2<-sort(x = twitter_tokens2,decreasing = TRUE)[1:10]
#top_blog2<-sort(x = blog_tokens2,decreasing = TRUE)[1:10]
#top_news2<-sort(x = news_tokens2,decreasing = TRUE)[1:10]
rm(blogdtm2,newsdtm2,twitterdtm2,BigramTokenizer)

##################
######Triigrams###

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
blogdtm3 <- TermDocumentMatrix(blogcorpus, control = list(tokenize = TrigramTokenizer))
newsdtm3<-TermDocumentMatrix(newscorpus, control = list(tokenize = TrigramTokenizer))
twitterdtm3<-TermDocumentMatrix(twittercorpus, control = list(tokenize = TrigramTokenizer))

sbt=strsplit(c(blogdtm3$dimnames$Terms,
                              newsdtm3$dimnames$Terms,
                              twitterdtm3$dimnames$Terms),split=" ")
df3=data.frame(t(do.call(cbind, sbt)))
for (i in 1:ncol(df3)){df3[,i]<-as.character(df3[,i])}
#df3$string<-paste(df3$X1,df3$X2)
#blog_tokens3<-rowSums(as.matrix(rollup(blogdtm3,2,na.rm=TRUE,FUN=sum)))
#news_tokens3<-rowSums(as.matrix(rollup(newsdtm3,2,na.rm=TRUE,FUN=sum)))
#twitter_tokens3<-rowSums(as.matrix(rollup(twitterdtm3,2,na.rm=TRUE,FUN=sum)))

#top_twitter3<-sort(x = twitter_tokens3,decreasing = TRUE)[1:10]
#top_blog3<-sort(x = blog_tokens3,decreasing = TRUE)[1:10]
#top_news3<-sort(x = news_tokens3,decreasing = TRUE)[1:10]
rm(blogdtm3,newsdtm3,twitterdtm3,TrigramTokenizer)

##################
######4gram#######

fourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
blogdtm4 <- TermDocumentMatrix(blogcorpus, control = list(tokenize = fourgramTokenizer))
newsdtm4<-TermDocumentMatrix(newscorpus, control = list(tokenize = fourgramTokenizer))
twitterdtm4<-TermDocumentMatrix(twittercorpus, control = list(tokenize = fourgramTokenizer))

sbt=strsplit(c(blogdtm4$dimnames$Terms,
               newsdtm4$dimnames$Terms,
               twitterdtm4$dimnames$Terms),split=" ")
df4=data.frame(t(do.call(cbind, sbt)))
df4$X4<-as.character(df4$X4)
for (i in 1:ncol(df4)){df4[,i]<-as.character(df4[,i])}
#df4$string<-paste(df4$X1,df4$X2,df4$X3)
#blog_tokens4<-rowSums(as.matrix(rollup(blogdtm4,2,na.rm=TRUE,FUN=sum)))
#news_tokens4<-rowSums(as.matrix(rollup(newsdtm4,2,na.rm=TRUE,FUN=sum)))
#twitter_tokens4<-rowSums(as.matrix(rollup(twitterdtm4,2,na.rm=TRUE,FUN=sum)))#

#top_twitter4<-sort(x = twitter_tokens4,decreasing = TRUE)[1:10]
#top_blog4<-sort(x = blog_tokens4,decreasing = TRUE)[1:10]
#top_news4<-sort(x = news_tokens4,decreasing = TRUE)[1:10]

#fivegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
#blogdtm5 <- TermDocumentMatrix(blogcorpus, control = list(tokenize = fivegramTokenizer))
#newsdtm5<-TermDocumentMatrix(newscorpus, control = list(tokenize = fivegramTokenizer))
#twitterdtm5<-TermDocumentMatrix(twittercorpus, control = list(tokenize = fivegramTokenizer))

#sbt=strsplit(c(blogdtm5$dimnames$Terms,
#               newsdtm5$dimnames$Terms,
#               twitterdtm5$dimnames$Terms),split=" ")
#df5=data.frame(t(do.call(cbind, sbt)))
#for (i in 1:ncol(df5)){df5[,i]<-as.character(df5[,i])}

#rm(blogdtm5,newsdtm5,twitterdtm5,fivegramTokenizer)

rm(blogdtm4,newsdtm4,twitterdtm4,fourgramTokenizer)
rm(sbt,clean_text,blogcorpus,newscorpus,twittercorpus,i)

df2=as.data.table(df2[df2$X1!=df2$X2,])
df3=as.data.table(df3[df3$X2!=df3$X3,])
df4=as.data.table(df4[df4$X3!=df4$X4,])
#df5=df5[df4$X4!=df4$X5,]


save.image("Cleaned Corpus + DTM.RData")
setwd(startwd)
rm(startwd)
