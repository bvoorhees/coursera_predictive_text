library(shiny) 

load("data/Cleaned Corpus + DTM.RData")

library(e1071)
library(data.table)
library(tm)

set.seed(123)
predict_text<-function(text,max=250){
          
          
          s5=0;s4=0; s3=0;s2=0
          
          text=tolower(text)
          text=gsub('[[:punct:]]',"",text)
          
          #quadgram pediction
          if(sapply(gregexpr("\\S+", text), length)>=3){
                    X<-as.matrix(df4[(df4[,X1]==tail(strsplit(text," ")[[1]],3)[1] &
                                      df4[,X2]==tail(strsplit(text," ")[[1]],3)[2] &
                                      df4[,X3]==tail(strsplit(text," ")[[1]],3)[3]),
                                     c("X1","X2","X3"),with=FALSE])
                    y<-as.factor(df4[(df4[,X1]==tail(strsplit(text," ")[[1]],3)[1] &
                                      df4[,X2]==tail(strsplit(text," ")[[1]],3)[2] &
                                      df4[,X3]==tail(strsplit(text," ")[[1]],3)[3]),
                                      c("X4"),with=FALSE]$X4)
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
                    X<-as.matrix(df3[(df3[,X1]==tail(strsplit(text," ")[[1]],2)[1] &
                                      df3[,X2]==tail(strsplit(text," ")[[1]],2)[2]),
                                      c("X1","X2"),with=FALSE])
                    if(nrow(X)>max){
                              X<-as.matrix(X[1:max,])
                    }
                    y<-as.factor(df3[(df3[,X1]==tail(strsplit(text," ")[[1]],2)[1] &
                                      df3[,X2]==tail(strsplit(text," ")[[1]],2)[2]),
                                      c("X3"),with=FALSE]$X3)
                    if(length(y)>max){
                              y<-as.factor(sample(x=df3[(df3[,X1]==tail(strsplit(text," ")[[1]],2)[1] &
                                                         df3[,X2]==tail(strsplit(text," ")[[1]],2)[2]),
                                                         c("X3"),with=FALSE]$X3,size=max,replace=TRUE))
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
                    X<-as.matrix(df2[df2[,X1]==strsplit(text," ")[[1]][1],X1])
                    if(length(X)>max){X<-as.matrix(X[1:max])}
                    y<-as.factor(df2[df2[,X1]==strsplit(text," ")[[1]][1],X2])
                    if(length(y)>max){y<-as.factor(sample(x=df2[df2[,X1]==strsplit(text," ")[[1]][1],X2],size=max,replace=TRUE))}
          }else{                    
                    X<-as.matrix(df2[df2[,X1]==tail(strsplit(text," ")[[1]],2)[1],X1])
                    if(length(X)>max){X<-as.matrix(X[1:max])}
                    y<-as.factor(df2[df2[,X1]==tail(strsplit(text," ")[[1]],2)[1],X2])
                    if(length(y)>max){y<-as.factor(sample(x=df2[df2[,X1]==tail(strsplit(text," ")[[1]],2)[1],X2],size=max,replace=TRUE))}
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
                    suggestion_raw<-sort(s,decreasing=TRUE)
                    names_suggestion1<-removeWords(names(suggestion_raw),stopwords("english"))
                    names_suggestion2<-removeNumbers(names_suggestion1)
                    suggestion_f1<-suggestion_raw[names(suggestion_raw)==names_suggestion2]
                    suggestion_f2<-names(suggestion_f1[!duplicated(names(suggestion_f1))][1:10])
                    suggestion<-suggestion_f2[!is.na(suggestion_f2) & suggestion_f2!=tail(strsplit(text," ")[[1]],1)[1]]
                    
                    return(suggestion)          
          }else{
                    return("Invalid Input")
          }
}


shinyServer(
          function(input, output) {
                    output$inputValue <- renderPrint({paste0(input$text)})
                    out<-reactive({predict_text(input$text)})
                    output$prediction <- renderPrint({out()[1]})
                    output$other_suggestions <- renderPrint({out()[2:10][!is.na(out()[2:10])]})
          }
)
