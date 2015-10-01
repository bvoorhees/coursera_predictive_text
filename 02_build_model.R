rm(list = ls())
start_load<-proc.time()
load("~/Dropbox/DSL/Coursera Data Science/Coursework/capstone/app/data/Cleaned Corpus + DTM.RData")
end_load<-proc.time()-start_load
end_load

library(e1071)
library(klaR)
library(caret)
library(data.table)
library(parallel)

set.seed(123)
predict_text<-function(text,max=250){
          
          s5=0;s4=0; s3=0;s2=0
          
          text=tolower(text)
          text=gsub('[[:punct:]]',"",text)
          
          #quadgram pediction
          if(sapply(gregexpr("\\S+", text), length)>=3){
                    X<-as.matrix(subset(df4[,1:3],df4[,1]==tail(strsplit(text," ")[[1]],3)[1] &
                                                  df4[,2]==tail(strsplit(text," ")[[1]],3)[2] &
                                                  df4[,3]==tail(strsplit(text," ")[[1]],3)[3]))
                    y<-as.factor(subset(df4[,4],df4[,1]==tail(strsplit(text," ")[[1]],3)[1] &
                                                  df4[,2]==tail(strsplit(text," ")[[1]],3)[2] &
                                                  df4[,3]==tail(strsplit(text," ")[[1]],3)[3]))
                    model = naiveBayes(x=X,y=y)
                    if(length(model$apriori)==1){
                              s4=model$apriori[1]
                    }else if(nrow(X)==0){
                              s4=0
                    }else{
                              s4<-sort(sapply(as.data.frame(predict(model, X, type = "raw")),max),decreasing = TRUE)
                    }
          }
          #trigram pediction
          if(sapply(gregexpr("\\S+", text), length)>=2){
                    X<-as.matrix(subset(df3[,1:2],df3[,1]==tail(strsplit(text," ")[[1]],2)[1] &
                                                  df3[,2]==tail(strsplit(text," ")[[1]],2)[2] ))
                    if(nrow(X)>max){
                              X<-X[1:max,]
                    }
                    y<-as.factor(subset(df3[,3],df3[,1]==tail(strsplit(text," ")[[1]],2)[1] &
                                                  df3[,2]==tail(strsplit(text," ")[[1]],2)[2] ))
                    if(length(y)>max){
                              y<-as.factor(sample(x=df3[(df3[,1]==tail(strsplit(text," ")[[1]],2)[1] &
                                                                   df3[,2]==tail(strsplit(text," ")[[1]],2)[2]),3] ,size=max,replace=TRUE))
                    }
                    model = naiveBayes(x=X,y=y)
                    if(length(model$apriori)==1){
                              s3=model$apriori[1]
                    }else if(nrow(X)==0){
                              s3=0
                    }else{
                              s3<-sort(sapply(as.data.frame(predict(model, X, type = "raw")),max),decreasing = TRUE)
                    }
          }
          #bigram prediction
          if(sapply(gregexpr("\\S+", text), length)==1){
                    X<-as.matrix(subset(df2[,1],df2[,1]==strsplit(text," ")[[1]][1]))
                    if(length(X)>max){X<-as.matrix(sample(x=X,size=max,replace=TRUE))}
                    y<-as.factor(subset(df2[,2],df2[,1]==strsplit(text," ")[[1]][1]))
                    if(length(y)>max){y<-as.factor(sample(x=df2[df2[,1]==strsplit(text," ")[[1]][1],2],size=max,replace=TRUE))}
          }else{                    
                    X<-as.matrix(subset(df2[,1],df2[,1]==tail(strsplit(text," ")[[1]],2)[2]))
                    if(length(X)>max){X<-as.matrix(sample(x=X,size=max,replace=TRUE))}
                    y<-as.factor(subset(df2[,2],df2[,1]==tail(strsplit(text," ")[[1]],2)[2]))
                    if(length(y)>max){y<-as.factor(sample(x=df2[df2[,1]==tail(strsplit(text," ")[[1]],2)[2],2],size=max,replace=TRUE))}
          }
          if (nrow(X)!=0 | length(y)!=0){
                    model = naiveBayes(x=X,y=y)
                    if(length(model$apriori)==1){
                              s2=model$apriori[1]
                    }else if(nrow(X)==0){
                              s2=0
                    }else{
                              s2<-sort(sapply(as.data.frame(predict(model, X, type = "raw")),max),decreasing = TRUE)
                    }
                    #top 5 suggestions based on conditional probability
                    
                    s<-c((s2*.1),(s3*.2),(s4*.7))
                    suggestion1<-sort(s,decreasing=TRUE)
                    suggestion<-names(suggestion1[!duplicated(names(suggestion1))][1:10])
                    suggestion<-suggestion[!is.na(suggestion)]
                    
                    return(suggestion)          
          }else{
                    return("Invalid Input")
          }
}