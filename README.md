# Machine-Learning

#            NAIVE BAYES
#     PHONE MESSAGE FILTERING


sms<-read.csv(file.choose(), stringsAsFactors = F)
sms

str(sms)
str(sms$v1)
table(sms$v1)

library(tm)
sms_corpus <-VCorpus(VectorSource(sms$v2))
sms_corpus
inspect(sms_corpus[1:3])
as.character(sms_corpus[[1]])
sms_corpus_clean<-tm_map(sms_corpus,content_transformer(tolower))
sms_corpus_clean
as.character(sms_corpus[[1]])
as.character(sms_corpus_clean[[1]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean,removePunctuation)
removePunctuation("hi.....Re")
replacePunctuation<-function(x){
 gsub("[[:punct:]]+"," ",x)
}
library(SnowballC)
wordStem(c("learn","learning", "learned","learns"))
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)

sms_dtm2<-TermDocumentMatrix(sms_corpus,control = list(tolower=T,
                                                      removeNumbers=T,
                                                      stopwords=T,
                                                      removePunctuation=T,
                                                      stem=T))
sms_dtm
##stopwords=function(x){removeWords(x,stopwords())}

sms_dtm_train<-sms_dtm[1:4175,]
sms_dtm_test<-sms_dtm[4176:5572,]
sms_train_labels<-sms[1:4175,]$v1
sms_test_labels<-sms[4176:5572,]$v1
prop.table(table(sms_train_labels))
prop.table(table(sms_test_labels))

library(wordcloud)
wordcloud(sms_corpus_clean,min.freq=50,random.order=F)
spam<-subset(sms,type ="spam")
ham<-subset(sms,type="ham")
wordcloud(spam$v2,max.words = 40,scale = c(3,0.5))
wordcloud(spam$v2,max.words = 40,scale = c(3,0.))

findFreqTerms(sms_dtm_train,5)
sms_Freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_Freq_words)
sms_dtm_freq_train<-sms_dtm_train[ , sms_Freq_words]
sms_dtm_freq_test<-sms_dtm_test[ , sms_Freq_words]
convert_counts<-function(x){
 x<-ifelse(x>0,"yes","no")
}
sms_train<-apply(sms_dtm_freq_train, MARGIN = 2,convert_counts)
sms_test<-apply(sms_dtm_freq_test, MARGIN = 2,convert_counts)
library(e1071)
sms_classifire<-naiveBayes(sms_train,sms_train_labels)
sms_classifire
sms_test_pred<-predict(sms_classifire,sms_test)
sms_test_pred
library(gmodels)
CrossTable(sms_test_pred,sms_test_labels,
           prop.chisq = F, prop.t = F,
           dnn=c('predicted','actual'))
